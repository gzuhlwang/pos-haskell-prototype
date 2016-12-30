{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Security.Workers
       ( SecurityWorkersClass (..)
       ) where

import           Control.Concurrent.STM            (TVar, newTVar, readTVar, writeTVar)
import           Control.Lens                      ((^.))
import           Control.Monad.Trans.Reader        (ReaderT (..), ask)
import qualified Data.HashMap.Strict               as HM
import           Data.Tagged                       (Tagged (..))
import           System.Wlog                       (logWarning)
import           Universum                         hiding (ask)

import           Pos.Block.Network.Request         (mkHeadersRequest)
import           Pos.Communication.Methods         (sendToNeighborsSafe)
import           Pos.Constants                     (k, mdNoBlocksSlotThreshold,
                                                    mdNoCommitmentsEpochThreshold)
import           Pos.Context                       (getNodeContext, ncPublicKey)
import           Pos.DB                            (getTipBlock, getTipBlockHeader,
                                                    loadBlocksFromTipWhile)
import           Pos.Slotting                      (onNewSlot)
import           Pos.Ssc.Class.Types               (Ssc (..))
import           Pos.Ssc.GodTossing.Types.Instance ()
import           Pos.Ssc.GodTossing.Types.Type     (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Types    (GtPayload (..), SscBi)
import           Pos.Ssc.NistBeacon                (SscNistBeacon)
import           Pos.Types                         (EpochIndex, MainBlock, SlotId (..),
                                                    blockMpc, flattenSlotId, gbHeader,
                                                    gbhConsensus, gcdEpoch, headerSlot,
                                                    headerHash)
import           Pos.Types.Address                 (addressHash)
import           Pos.WorkMode                      (WorkMode)

class Ssc ssc => SecurityWorkersClass ssc where
    securityWorkers :: WorkMode ssc m => Tagged ssc [m ()]

instance SscBi => SecurityWorkersClass SscGodTossing where
    securityWorkers = Tagged [ checkForReceivedBlocksWorker
                             , checkForIgnoredCommitmentsWorker
                             ]

instance SecurityWorkersClass SscNistBeacon where
    securityWorkers = Tagged [ checkForReceivedBlocksWorker
                             ]

reportAboutEclipsed :: WorkMode ssc m => m ()
reportAboutEclipsed = logWarning "We're doomed, we're eclipsed!"

requestNewHeaders :: WorkMode ssc m => m ()
requestNewHeaders = sendToNeighborsSafe =<< mkHeadersRequest . Just . headerHash =<< getTipBlockHeader

checkForReceivedBlocksWorker :: WorkMode ssc m => m ()
checkForReceivedBlocksWorker = onNewSlot True $ \slotId -> do
    headBlock <- getTipBlock
    case headBlock of
        Left genesis -> compareSlots slotId $ SlotId (genesis ^. gbHeader . gbhConsensus . gcdEpoch) 0
        Right blk    -> compareSlots slotId (blk ^. gbHeader . headerSlot)
  where
    compareSlots slotId blockGeneratedId = do
        let fSlotId = flattenSlotId slotId
        let fBlockGeneratedSlotId = flattenSlotId blockGeneratedId
        when (fSlotId - fBlockGeneratedSlotId > mdNoBlocksSlotThreshold) $ do
            reportAboutEclipsed
            requestNewHeaders

checkForIgnoredCommitmentsWorker :: forall m. WorkMode SscGodTossing m => m ()
checkForIgnoredCommitmentsWorker = do
    epochIdx <- atomically (newTVar 0)
    _ <- runReaderT (onNewSlot True checkForIgnoredCommitmentsWorkerImpl) epochIdx
    return ()

checkForIgnoredCommitmentsWorkerImpl
    :: forall m. WorkMode SscGodTossing m
    => SlotId -> ReaderT (TVar EpochIndex) m ()
checkForIgnoredCommitmentsWorkerImpl slotId = do
    checkCommitmentsInPreviousBlocks slotId
    tvar <- ask
    lastCommitment <- lift $ atomically $ readTVar tvar
    when (siEpoch slotId - lastCommitment > mdNoCommitmentsEpochThreshold) $ do
        lift reportAboutEclipsed

checkCommitmentsInPreviousBlocks
    :: forall m. WorkMode SscGodTossing m
    => SlotId -> ReaderT (TVar EpochIndex) m ()
checkCommitmentsInPreviousBlocks slotId = do
    kBlocks <- map fst <$> loadBlocksFromTipWhile (\_ depth -> depth < k)
    forM_ kBlocks $ \case
        Right blk -> checkCommitmentsInBlock slotId blk
        _         -> return ()

checkCommitmentsInBlock
    :: forall m. WorkMode SscGodTossing m
    => SlotId -> MainBlock SscGodTossing -> ReaderT (TVar EpochIndex) m ()
checkCommitmentsInBlock slotId block = do
    ourId <- addressHash . ncPublicKey <$> getNodeContext
    let commitmentInBlockchain = isCommitmentInPayload ourId (block ^. blockMpc)
    when commitmentInBlockchain $ do
        tvar <- ask
        lift $ atomically $ writeTVar tvar $ siEpoch slotId
  where
    isCommitmentInPayload addr (CommitmentsPayload commitments _) = HM.member addr commitments
    isCommitmentInPayload _ _ = False
