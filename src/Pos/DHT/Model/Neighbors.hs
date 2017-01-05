{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE OverloadedStrings         #-}

module Pos.DHT.Model.Neighbors where

import           Control.Monad.Catch                    (catch)
import           Control.Monad                          (sequence)

import           Pos.DHT.Model.Types                    (DHTNodeType(..))
import           Pos.DHT.Model.Class.BiP
import           Pos.DHT.Model.Class.MonadDHT           (MonadDHT)
import           Pos.Constants                          (neighborsSendThreshold)
import           Node                                   (SendActions(..))
import           Message.Message                        (Serializable)
import           Mockable.Monad                         (MonadMockable)
import           Network.Discovery.Transport.Kademlia   -- For discoverPeers?

-- | Send default message to neighbours in parallel.
sendToNeighbors
    :: ( MonadDHT m, MonadMockable m, Serializable packing body )
    => SendActions packing m
    -> body
    -> m ()
sendToNeighbors sender msg = do
    nodes <- filterByNodeType DHTFull <$> getKnownPeers
    succeed <- sendToNodes nodes
    succeed' <-
        if succeed < neighborsSendThreshold
            then (+) succeed <$>
                 do nodes' <- discoverPeers DHTFull
                    let newNodes = filter (flip notElem nodes) nodes'
                    sendToNodes newNodes
            else return succeed
    when (succeed' < neighborsSendThreshold) $
        logWarning $ sformat ("Send to only " % int % " nodes, threshold is " % int) succeed' (neighborsSendThreshold :: Int)
    return succeed'
  where
    sendToNodes nodes = length . filter identity <$> sequence (map send' nodes) -- TODO: Should we use 'sequence' explicitly?
    send' node = sendToOneNode `catch` handleE
      where
        sendToOneNode = do
            let (host, port) = dhtAddr node
                anId = NodeId $ host <> ":" <> port -- TODO: What about node index, i.e. last number in 127.0.0.1:3000:0 ?
            -- Probably 'sender' is a 'sendTo' from node-sketch.
            sender anId (messageName msg) msg 
            return True
                
        handleE (e :: SomeException) = do
            logInfo $ sformat ("Error sending message to " % F.build % ": " % shown) node e
            return False

-- ListenerActionOneMsg
--     :: ( Serializable packing msg )
--     => (LL.NodeId -> SendActions packing m -> msg -> m ())
--     -> ListenerAction packing m

-- | Send a isolated (sessionless) message to a node
--        sendTo :: forall body .
--               ( Packable packing body )
--               => LL.NodeId
--               -> MessageName
--               -> body
--               -> m (),
