{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Server which handles transactions.

module Pos.Txp.Listeners
       ( txListeners
       , processTx
       ) where

import qualified Data.HashMap.Strict         as HM
import           Formatting                  (build, sformat, stext, (%))
import           Serokell.Util.Verify        (VerificationRes (..))
import           System.Wlog                 (logDebug, logInfo, logWarning)
import           Universum

import           Pos.Binary.Relay            ()
import           Pos.Communication.Types     (MutSocketState, ResponseMode)
import           Pos.Crypto                  (hash)
import           Pos.DHT.Model               (ListenerDHT (..), MonadDHTDialog)
import           Pos.Statistics              (StatProcessTx (..), statlogCountEvent)
import           Pos.Txp.Class               (getMemPool)
import           Pos.Txp.Logic               (processTx)
import           Pos.Txp.Types.Communication (TxMsgContents (..), TxMsgTag (..))
import           Pos.Txp.Types.Types         (MemPool (..), ProcessTxRes (..))
import           Pos.Types                   (TxAux, TxId)
import           Pos.Util.Relay              (DataMsg, InvMsg, Relay (..), ReqMsg,
                                              handleDataL, handleInvL, handleReqL)
import           Pos.WorkMode                (WorkMode)

-- | Listeners for requests related to blocks processing.
txListeners
    :: (MonadDHTDialog (MutSocketState ssc) m, WorkMode ssc m)
    => [ListenerDHT (MutSocketState ssc) m]
txListeners =
    [
      ListenerDHT handleInvTx
    , ListenerDHT handleReqTx
    , ListenerDHT handleDataTx
    ]

handleInvTx :: ResponseMode ssc m => InvMsg TxId TxMsgTag -> m ()
handleInvTx = handleInvL

handleReqTx :: ResponseMode ssc m => ReqMsg TxId TxMsgTag -> m ()
handleReqTx = handleReqL

handleDataTx :: ResponseMode ssc m => DataMsg TxId TxMsgContents -> m ()
handleDataTx = handleDataL

instance ( WorkMode ssc m
         ) => Relay m TxMsgTag TxId TxMsgContents where
    contentsToTag _ = pure TxMsgTag

    verifyInvTag _ = pure VerSuccess
    verifyReqTag _ = pure VerSuccess
    verifyDataContents _ = pure VerSuccess

    handleInv _ txId = not . HM.member txId  . localTxs <$> getMemPool

    handleReq _ txId = fmap toContents . HM.lookup txId . localTxs <$> getMemPool
      where
        toContents (tx, tw, td) = TxMsgContents tx tw td

    handleData (TxMsgContents tx tw td) _ = handleTxDo (hash tx, (tx, tw, td))

-- Real tx processing
-- CHECK: @handleTxDo
-- #processTx
handleTxDo
    :: WorkMode ssc m
    => (TxId, TxAux) -> m Bool
handleTxDo tx = do
    res <- processTx tx
    let txId = fst tx
    case res of
        PTRadded -> do
            statlogCountEvent StatProcessTx 1
            logInfo $
                sformat ("Transaction has been added to storage: "%build) txId
        PTRinvalid msg ->
            logWarning $
            sformat ("Transaction "%build%" failed to verify: "%stext) txId msg
        PTRknown ->
            logDebug $ sformat ("Transaction is already known: "%build) txId
        PTRoverwhelmed ->
            logInfo $ sformat ("Node is overwhelmed, can't add tx: "%build) txId
    return (res == PTRadded)
