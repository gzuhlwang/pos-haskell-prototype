{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE OverloadedStrings         #-}

module Pos.DHT.Model.Neighbors where

import           Control.Monad.Catch                    (SomeException, catch)
import           Control.Monad                          (sequence)
import           Data.ByteString.Char8                  (pack)

import           Universum
import           Pos.DHT.Model.Types                    (DHTNodeType(..))
import           Pos.DHT.Model.Class.BiP
import           Pos.DHT.Model.Class.MonadDHT           (MonadDHT)
import           Pos.Constants                          (neighborsSendThreshold)
import           Node                                   (SendActions(..))
import           Message.Message                        (Serializable)
import           Control.TimeWarp.Rpc                   (messageName)
import           Mockable.Monad                         (MonadMockable)
import           Network.Discovery.Transport.Kademlia   -- For discoverPeers?
import           Formatting                             (int, sformat, shown, (%))
import qualified Formatting                             as F

-- | Send default message to neighbours in parallel.
-- It's a broadcasting to the neighbours without sessions
-- (i.e. we don't have to wait for reply from the listeners).
sendToNeighbors
    :: ( MonadDHT m, MonadMockable m, Serializable packing body )
    => SendActions packing m
    -> body
    -> m ()
sendToNeighbors sender msg = do
    --nodes <- filterByNodeType DHTFull <$> getKnownPeers
    --succeed <- sendToNodes nodes
    --succeed' <-
    --    if succeed < neighborsSendThreshold
    --        then (+) succeed <$>
    --             do nodes' <- discoverPeers DHTFull
    --                let newNodes = filter (flip notElem nodes) nodes'
    --                sendToNodes newNodes
    --        else return succeed
    --when (succeed' < neighborsSendThreshold) $
    --    logWarning $ sformat ("Send to only " % int % " nodes, threshold is " % int) succeed' neighborsSendThreshold
    --return succeed'
    return ()
  --where
    --sendToNodes nodes = length . filter identity <$> sequence (map send' nodes) -- TODO: Should we use 'sequence' explicitly?
    --send' node = sendToOneNode `catch` handleE
    --  where
    --    sendToOneNode = do
    --        let (host, port) = dhtAddr node
    --            anId = NodeId $ host <> ":" <> (pack . show $ port) -- TODO: What about node index, i.e. last number in '127.0.0.1:3000:0' ?
    --        sender anId (messageName msg) msg 
    --        return True
                
    --    handleE (e :: SomeException) = do
    --        logInfo $ sformat ("Error sending message to " % F.build % ": " % shown) node e
    --        return False
