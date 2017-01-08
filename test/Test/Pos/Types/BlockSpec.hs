{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Specification of Pos.Types.Block.

module Test.Pos.Types.BlockSpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe, it)
import           Test.Hspec.QuickCheck (prop)
import           Universum

import           Control.Lens          (makeLenses, (^.))
import           Control.Monad         (replicateM)
import           Data.Ix               (range)
import           Pos.Block.Arbitrary   as T
import           Pos.Constants         (epochSlots)
import           Pos.Crypto            (ProxySecretKey (..), SecretKey,
                                        createProxySecretKey, hash, sign, toPublic)
import           Pos.Ssc.Class.Types   (Ssc (..))
import           Pos.Ssc.GodTossing    (SscGodTossing)
import           Pos.Ssc.NistBeacon    (SscNistBeacon)
import qualified Pos.Types             as T
import           Serokell.Util         (isVerSuccess)

import           Test.QuickCheck       (Arbitrary (..), Gen, Property, choose, oneof,
                                        vectorOf, (==>))

spec :: Spec
spec = describe "Block properties" $ do
    describe "verifyHeader" $ do
        prop verifyHeaderDesc (validateGoodMainHeader @SscNistBeacon)
        prop verifyHeaderDesc (validateGoodMainHeader @SscGodTossing)
    describe "verifyHeaders" $ do
        prop verifyHeadersDesc (validateGoodHeaderChain @SscNistBeacon)
        prop verifyHeadersDesc (validateGoodHeaderChain @SscGodTossing)
        it verifyEmptyHsDesc $
            and
                [isVerSuccess $
                    T.verifyHeaders bool ([] :: [T.BlockHeader SscNistBeacon])
                        | bool <- [False, True]] == True
        it verifyEmptyHsDesc $
            and
                [isVerSuccess $
                    T.verifyHeaders bool ([] :: [T.BlockHeader SscGodTossing])
                        | bool <- [False, True]] == True
          where
    verifyHeaderDesc = "Successfully verifies a correct main block header"
    verifyHeadersDesc = "Successfully verifies a correct chain of block headers"
    verifyEmptyHsDesc = "Successfully validates an empty header chain"

----------------------------------------------------------------------------
-- GenesisBlock ∪ MainBlock
----------------------------------------------------------------------------

startingEpoch :: Integral a => a
startingEpoch = 0

maxEpochs :: Integral a => a
maxEpochs = 10

recursiveHeaderGen
    :: (Arbitrary (SscPayload ssc), Ssc ssc, Integral a)
    => [Either SecretKey (SecretKey, SecretKey)]
    -> [(a, a)]
    -> [T.BlockHeader ssc]
    -> Gen [T.BlockHeader ssc]
recursiveHeaderGen
    (eitherOfLeader : leaders)
    ((epoch, slot) : rest)
    blockchain@(prevHeader : _) = do
        let epochCounter = fromIntegral epoch
            slotCounter = fromIntegral slot
        curHeader <- do
            if slot == epochSlots
                then do
                    body <- arbitrary
                    return $ Left $
                        T.mkGenesisHeader
                            (Just prevHeader) (epochCounter + 1) body
                else do
                    body <- arbitrary
                    extraHData <- arbitrary
                    let slotId = T.SlotId epochCounter slotCounter
                        (curLeader, proxySK) =
                            case eitherOfLeader of
                                Left sk -> (sk, Nothing)
                                Right (issuerSK, delegateSK) ->
                                    let w = (epochCounter, epochCounter + 3)
                                        delegatePK = toPublic delegateSK
                                        proxy =
                                            createProxySecretKey issuerSK delegatePK w
                                    in (delegateSK, Just $ Left proxy)
                    return $ Right $
                        T.mkMainHeader
                            (Just prevHeader) slotId curLeader proxySK body extraHData
        recursiveHeaderGen leaders rest (curHeader : blockchain)
recursiveHeaderGen [] _ b = return b
recursiveHeaderGen _ [] b = return b
recursiveHeaderGen _ _ [] = return []

instance (Arbitrary (SscPayload ssc), Ssc ssc) => Arbitrary (BlockHeaderList ssc) where
    arbitrary = BHL <$> do
        fullEpochs <- choose (startingEpoch, maxEpochs)
        incompleteEpochSize <- T.LocalSlotIndex <$> choose (1, epochSlots - 1)
        leadersList <-
            vectorOf (((epochSlots - 1) * fullEpochs) + fromIntegral incompleteEpochSize)
                arbitrary
        firstGenesisBody <- arbitrary
        let firstHeader = Left $ T.mkGenesisHeader Nothing startingEpoch firstGenesisBody
        recursiveHeaderGen
            leadersList
            (range ((startingEpoch, 0), (fromIntegral fullEpochs, epochSlots)) ++
            zip (repeat $ fromIntegral (fullEpochs + 1))
                [0 .. incompleteEpochSize - 1])
            [firstHeader]

validateGoodMainHeader
    :: forall ssc . Ssc ssc
    => T.BlockHeaderList ssc -> Bool
validateGoodMainHeader (T.getHeaderList -> l) =
    let atMost3Blocks = take 3 l
        (prev, block, next) =
            case atMost3Blocks of
                [b] -> (Nothing, b, Nothing)
                [b2, b1] -> (Just b1, b1, Nothing)
                (b3 : b2 : b1 : _) -> (Just b1, b2, Just b3)
                _ -> panic "[BlockSpec] the blockchain doesn't have enough blocks"
        params = T.VerifyHeaderParams
            { T.vhpVerifyConsensus = True
            , T.vhpPrevHeader = prev
            , T.vhpNextHeader = next
            , T.vhpCurrentSlot = Nothing
            , T.vhpLeaders = Nothing
            }
    in isVerSuccess $ T.verifyHeader params block

validateGoodHeaderChain
    :: forall ssc . Ssc ssc
    => T.BlockHeaderList ssc -> Bool
validateGoodHeaderChain (T.getHeaderList -> l) =
    isVerSuccess $ T.verifyHeaders True l
