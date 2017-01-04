{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Server which handles transactions.

module Pos.Txp.Listeners
       ( txListeners
       , processTx
       ) where

import qualified Data.HashMap.Strict         as HM
import qualified Data.List.NonEmpty          as NE
import           Formatting                  (build, sformat, stext, (%))
import           Serokell.Util.Verify        (VerificationRes (..))
import           System.Wlog                 (logDebug, logInfo, logWarning)
import           Universum

import           Pos.Binary.Relay            ()
import           Pos.Communication.Methods   (sendToNeighborsSafe)
import           Pos.Communication.Types     (MutSocketState, ResponseMode)
import           Pos.Context                 (WithNodeContext (getNodeContext),
                                              ncPropagation)
import           Pos.Crypto                  (hash)
import           Pos.DHT.Model               (Listener (..), MonadDHTDialog,
                                              replyToNode)
import           Pos.Statistics              (StatProcessTx (..), statlogCountEvent)
import           Pos.Txp.Class               (MonadTxpLD, getMemPool)
import           Pos.Txp.Logic               (processTx)
import           Pos.Txp.Types.Communication (TxMsgContents (..), TxMsgTag (..))
import           Pos.Txp.Types.Types         (MemPool (..), ProcessTxRes (..), TxMap)
import           Pos.Types                   (TxAux, TxId)
import           Pos.Util.Relay              (DataMsg, InvMsg, Relay (..), ReqMsg,
                                              handleDataL, handleInvL, handleReqL)
import           Pos.WorkMode                (WorkMode)
import           Node
import           Message.Message             (BinaryP (..))

-- | Listeners for requests related to blocks processing.
txListeners
    :: (MonadDHTDialog (MutSocketState ssc) m, WorkMode ssc m)
    => [Listener () BinaryP m]
txListeners =
    [
      Listener (messageName (Proxy :: Proxy (TxInvMsg ssc)))  handleTxInv
    , Listener (messageName (Proxy :: Proxy (TxReqMsg ssc)))  handleTxReq
    , Listener (messageName (Proxy :: Proxy (TxDataMsg ssc))) handleTxData
    ]




{-
handleTxInv :: (ResponseMode ssc m)
            => ListenerAction () BinaryP m
handleTxInv = ListenerActionConversation $
    \_ (actions :: ConversationActions () (TxReqMsg ssc) TxInvMsg m) -> do
        let ConversationActions _ _ (TxInvMsg (NE.toList -> txHashes)) _ = actions
        added <- mapM handleSingle txHashes
        let addedItems = map snd . filter fst . zip added $ txHashes
        safeReply addedItems
      where
        safeReply = maybe pass (\r -> send actions () $ TxReqMsg r) . NE.nonEmpty
        handleSingle txHash =
            ifM (isTxUseful txHash)
                (True <$ requestingLogMsg txHash)
                (False <$ ingoringLogMsg txHash)
        requestingLogMsg txHash = logDebug $
            sformat ("Requesting tx with hash "%build) txHash
        ingoringLogMsg txHash = logDebug $
            sformat ("Ignoring tx with hash ("%build%"), because it's useless") txHash
-}

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
