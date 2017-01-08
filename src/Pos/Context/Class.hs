{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Class which provides access to NodeContext.

module Pos.Context.Class
       ( WithNodeContext (..)

       , putBlkSemaphore
       , readBlkSemaphore
       , takeBlkSemaphore

       , readLeaders
       , readLeadersExpEpoch
       , tryReadLeadersExpEpoch
       , writeLeaders
       , isLeadersComputed
       ) where

import           Control.Concurrent.MVar (putMVar)
import           Universum

import           Pos.Context.Context     (NodeContext (..))
import           Pos.DHT.Model           (DHTResponseT)
import           Pos.DHT.Real            (KademliaDHT)
import           Pos.Types               (HeaderHash, SlotLeaders, EpochIndex)

-- | Class for something that has 'NodeContext' inside.
class WithNodeContext ssc m | m -> ssc where
    getNodeContext :: m (NodeContext ssc)

instance (Monad m, WithNodeContext ssc m) =>
         WithNodeContext ssc (KademliaDHT m) where
    getNodeContext = lift getNodeContext

instance (Monad m, WithNodeContext ssc m) =>
         WithNodeContext ssc (ReaderT a m) where
    getNodeContext = lift getNodeContext

instance (Monad m, WithNodeContext ssc m) =>
         WithNodeContext ssc (StateT a m) where
    getNodeContext = lift getNodeContext

instance (Monad m, WithNodeContext ssc m) =>
         WithNodeContext ssc (DHTResponseT s m) where
    getNodeContext = lift getNodeContext


----------------------------------------------------------------------------
-- Semaphore-related logic
----------------------------------------------------------------------------

takeBlkSemaphore
    :: (MonadIO m, WithNodeContext ssc m)
    => m (HeaderHash ssc)
takeBlkSemaphore = liftIO . takeMVar . ncBlkSemaphore =<< getNodeContext

putBlkSemaphore
    :: (MonadIO m, WithNodeContext ssc m)
    => HeaderHash ssc -> m ()
putBlkSemaphore tip = liftIO . flip putMVar tip . ncBlkSemaphore =<< getNodeContext

readBlkSemaphore
    :: (MonadIO m, WithNodeContext ssc m)
    => m (HeaderHash ssc)
readBlkSemaphore = liftIO . readMVar . ncBlkSemaphore =<< getNodeContext

----------------------------------------------------------------------------
-- LRC data
----------------------------------------------------------------------------

-- | Read slot leaders from node context. This function blocks if
-- leaders are not available.
readLeaders
    :: (MonadIO m, WithNodeContext ssc m)
    => m (EpochIndex, SlotLeaders)
readLeaders = getNodeContext >>= liftIO . readMVar . ncSscLeaders

readLeadersExpEpoch
    :: (MonadIO m, WithNodeContext ssc m)
    => EpochIndex -> m SlotLeaders
readLeadersExpEpoch expEpoch = do
    nc <- getNodeContext
    (epoch, leaders) <- liftIO . readMVar $ ncSscLeaders nc
    if epoch == expEpoch then
        pure leaders
    else
        readLeadersExpEpoch expEpoch

-- | Read slot leaders from node context. Nothing is returned if
-- leaders are not available.
tryReadLeadersExpEpoch
    :: (MonadIO m, WithNodeContext ssc m)
    => EpochIndex -> m (Maybe SlotLeaders)
tryReadLeadersExpEpoch epoch = do
    nc <- getNodeContext
    leaders <- liftIO . tryReadMVar $ ncSscLeaders nc
    pure $ maybe Nothing (\(e, l) -> if e == epoch then Just l else Nothing) leaders

-- | Put leaders into MVar, assuming it's empty.
writeLeaders
    :: (MonadIO m, WithNodeContext ssc m)
    => EpochIndex -> SlotLeaders -> m ()
writeLeaders epoch leaders = getNodeContext >>=
    liftIO . flip putMVar (epoch, leaders) . ncSscLeaders

isLeadersComputed
    :: (MonadIO m, WithNodeContext ssc m)
    => m Bool
isLeadersComputed = not <$> (getNodeContext >>= liftIO . isEmptyMVar . ncSscLeaders)
