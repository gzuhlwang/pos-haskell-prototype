{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Pos.DHT.Real.Real
       ( runKademliaDHT
       , startDHTInstance
       , stopDHTInstance

       -- * For Servant integration
       , getKademliaDHTCtx
       , runKademliaDHTRaw
       ) where

import           Control.Concurrent.Async.Lifted (async, mapConcurrently)
import           Control.Concurrent.STM          (STM, TVar, modifyTVar, newTVar,
                                                  readTVar, swapTVar, writeTVar)
import           Control.Monad.Catch             (Handler (..), MonadCatch, MonadMask,
                                                  MonadThrow, catchAll, catches, throwM)
import           Control.Monad.Trans.Control     (MonadBaseControl)
import           Control.TimeWarp.Rpc            (Binding (..), ListenerH (..),
                                                  NetworkAddress, RawData (..),
                                                  TransferException (..), listenR, sendH,
                                                  sendR)
import           Control.TimeWarp.Timed          (ms, sec)
import           Mockable.Concurrent             (fork, killThread)
import           Mockable.Monad                  (MonadMockable)

import qualified Data.Cache.LRU                  as LRU
import           Data.Hashable                   (hash)
import qualified Data.HashMap.Strict             as HM
import           Data.List                       (intersect, (\\))

import           Formatting                      (build, int, sformat, shown, (%))
import qualified Network.Kademlia                as K
import           Prelude                         (id)
import           System.Wlog                     (WithLogger, getLoggerName, logDebug,
                                                  logError, logInfo, logWarning,
                                                  usingLoggerName)
import           Universum                       hiding (async, fromStrict,
                                                  mapConcurrently, toStrict)

import           Pos.Binary.Class                (Bi (..))
import           Pos.Binary.DHTModel             ()
import           Pos.Constants                   (enchancedMessageBroadcast)
import           Pos.DHT.Model.Class             (DHTException (..), DHTMsgHeader (..),
                                                  DHTPacking, DHTResponseT (..),
                                                  ListenerDHT (..), MonadDHT (..),
                                                  MonadDHTDialog,
                                                  MonadResponseDHT (closeResponse),
                                                  defaultSendToNeighbors,
                                                  defaultSendToNode, withDhtLogger)
import           Pos.DHT.Model.Types             (DHTData, DHTKey, DHTNode (..),
                                                  filterByNodeType, randomDHTKey)
import           Pos.DHT.Model.Util              (joinNetworkNoThrow)
import           Pos.DHT.Real.Types              (DHTHandle, KademliaDHT (..),
                                                  KademliaDHTConfig (..),
                                                  KademliaDHTContext (..),
                                                  KademliaDHTInstance (..),
                                                  KademliaDHTInstanceConfig (..))
import           Pos.Util                        (runWithRandomIntervals,
                                                  waitAnyUnexceptional)

import           Node                            (startNode)

kademliaConfig :: K.KademliaConfig
kademliaConfig = K.defaultConfig { K.k = 16 }

-- | Run 'KademliaDHT' with provided 'KademliaDHTContext'
runKademliaDHTRaw :: KademliaDHTContext m -> KademliaDHT m a -> m a
runKademliaDHTRaw ctx action = runReaderT (unKademliaDHT action) ctx

-- | Get context from 'KademliaDHT'
getKademliaDHTCtx :: Monad m => KademliaDHT m (KademliaDHTContext m)
getKademliaDHTCtx = KademliaDHT ask

-- | Run 'KademliaDHT' with provided 'KademliaDTHConfig'.
runKademliaDHT
    :: ( WithLogger m
       , MonadIO m
       , MonadMockable m
       , MonadDHTDialog s m
       , MonadMask m
       , MonadBaseControl IO m
       )
    => KademliaDHTConfig s m -> KademliaDHT m a -> m a
runKademliaDHT kdc@(KademliaDHTConfig {..}) action =
    startDHT kdc >>= runReaderT (unKademliaDHT action')
  where
    action' =
        action''
        `finally`
        (logDebug "stopping kademlia messager"
          >> stopDHT >> logDebug "kademlia messager stopped")
    action'' = do
      startMsgThread
      logDebug "running kademlia dht messager"
      joinNetworkNoThrow (kdiInitialPeers $ kdcDHTInstance)
      startRejoinThread
      action
    startRejoinThread = do
      tvar <- KademliaDHT $ asks kdcAuxClosers
      tid <- fork $ runWithRandomIntervals (ms 500) (sec 5) rejoinNetwork
      atomically $ modifyTVar tvar (killThread tid:)
    startMsgThread = do
      (tvar, listenByBinding) <-
          KademliaDHT $ (,) <$> asks kdcAuxClosers <*> asks kdcListenByBinding
      closer <- listenByBinding $ AtPort kdcPort
      atomically $ modifyTVar tvar (closer:)

-- | Stop DHT algo.
stopDHT :: (MonadIO m, MonadMockable m) => KademliaDHT m ()
stopDHT = do
    (closersTV, stoppedTV) <- KademliaDHT $ (,)
            <$> asks kdcAuxClosers
            <*> asks kdcStopped
    atomically $ writeTVar stoppedTV True
    closers <- atomically $ swapTVar closersTV []
    sequence_ closers

-- | Stop chosen 'KademliaDHTInstance'.
stopDHTInstance
    :: MonadIO m
    => KademliaDHTInstance -> m ()
stopDHTInstance KademliaDHTInstance {..} = liftIO $ K.close kdiHandle

-- | Start 'KademliaDHTInstance' with 'KademliaDHTInstanceConfig'.
startDHTInstance
    :: ( MonadIO m
       , MonadMockable m
       , WithLogger m
       , MonadCatch m
       , MonadBaseControl IO m
       , Bi DHTData
       , Bi DHTKey
       )
    => KademliaDHTInstanceConfig -> m KademliaDHTInstance
startDHTInstance KademliaDHTInstanceConfig {..} = do
    kdiKey <- either pure randomDHTKey kdcKeyOrType
    kdiHandle <-
        (liftIO $
        K.createL
            (fromInteger . toInteger $ kdcPort)
            kdiKey
            kademliaConfig
            (log' logDebug)
            (log' logError))
          `catchAll`
        (\e ->
           do logError $ sformat
                  ("Error launching kademlia at port " % int % ": " % shown) kdcPort e
              throwM e)
    let kdiInitialPeers = kdcInitialPeers
    let kdiExplicitInitial = kdcExplicitInitial
    kdiKnownPeersCache <- atomically $ newTVar []
    pure $ KademliaDHTInstance {..}
  where
    log' logF =  usingLoggerName ("kademlia" <> "messager") . logF . toText

startDHT
    :: ( MonadIO m
       , MonadMockable m
       , MonadDHTDialog s m
       , WithLogger m
       , MonadMask m
       , MonadBaseControl IO m
       )
    => KademliaDHTConfig s m -> m (KademliaDHTContext m)
startDHT KademliaDHTConfig {..} = do
    kdcStopped <- atomically $ newTVar False
    kdcAuxClosers <- atomically $ newTVar []
    msgCache <- atomically $
        newTVar (LRU.newLRU (Just $ toInteger kdcMessageCacheSize) :: LRU.LRU Int ())
    let kdcListenByBinding binding = do
            closer <- listenR binding
                              (convert <$> kdcListeners)
                              (convert' $ rawListener kdcEnableBroadcast msgCache kdcStopped)
            -- startNode
            --       NT.EndPoint m
            --    -> StdGen
            --    -> [] -- Empty list of workers, here we start listeners only.
            --    -> Maybe (PreListener header m)
            --    -> [Listener header m]
            --    -> m (Node m)
            logInfo $ sformat ("Listening on binding " % shown) binding
            return closer
    logInfo $ sformat ("Launching Kademlia, noCacheMessageNames=" % shown) kdcNoCacheMessageNames
    let kdcNoCacheMessageNames_ = kdcNoCacheMessageNames
    let kdcDHTInstance_ = kdcDHTInstance
    pure $ KademliaDHTContext {..}
  where
    convert :: ListenerDHT s m -> ListenerH s DHTPacking DHTMsgHeader m
    convert (ListenerDHT f) = ListenerH $ \(_, m) -> getDHTResponseT $ f m
    convert' handler = getDHTResponseT . handler

-- | Return 'True' if the message should be processed, 'False' if only
-- broadcasted
rawListener
    :: ( MonadBaseControl IO m
       , MonadMask m
       , MonadDHTDialog s m
       , MonadIO m
       , MonadMockable m
       , WithLogger m
       )
    => Bool
    -> TVar (LRU.LRU Int ())
    -> TVar Bool
    -> (DHTMsgHeader, RawData)
    -> DHTResponseT s (KademliaDHT m) Bool
rawListener enableBroadcast cache kdcStopped (h, rawData@(RawData raw)) = withDhtLogger $ do
    isStopped <- atomically $ readTVar kdcStopped
    when isStopped $ do
        closeResponse
        throwM $ FatalError "KademliaDHT stopped"
    let mHash = hash raw
    ignoreMsg <- case h of
                   SimpleHeader True -> return False
                   _                 -> atomically $ updCache cache mHash
    if ignoreMsg
       then logDebug $
                sformat ("Ignoring message " % shown % ", hash=" % int) h mHash
       else return ()

    -- If the message is in cache, we have already broadcasted it before, no
    -- need to do it twice
    when (not ignoreMsg && enableBroadcast) $
        case h of
            BroadcastHeader -> do
              logDebug $
                sformat ("Broadcasting message " % shown % ", hash=" % int) h mHash
              lift $ sendToNetworkR rawData
            SimpleHeader _  -> pure ()
    -- If the message wasn't in the cache, we want to process it too (not
    -- simply broadcast it)
    return (not ignoreMsg)

updCache :: TVar (LRU.LRU Int ()) -> Int -> STM Bool
updCache cacheTV dataHash = do
    cache <- readTVar cacheTV
    let (cache', mP) = dataHash `LRU.lookup` cache
    case mP of
      Just _ -> writeTVar cacheTV cache' >> pure True
      _      -> writeTVar cacheTV (LRU.insert dataHash () cache') >> pure False

sendToNetworkR
    :: ( MonadBaseControl IO m
       , WithLogger m
       , MonadCatch m
       , MonadIO m
       , MonadMockable m
       , MonadDHTDialog s m 
       ) => RawData -> KademliaDHT m ()
sendToNetworkR = sendToNetworkImpl sendR

sendToNetworkImpl
    :: ( MonadBaseControl IO m
       , WithLogger m
       , MonadCatch m
       , MonadIO m
       , MonadMockable m
       , MonadDHTDialog s m
       )
    => (NetworkAddress -> DHTMsgHeader -> msg -> KademliaDHT m ())
    -> msg
    -> KademliaDHT m ()
sendToNetworkImpl sender msg = do
    logDebug "Sending message to network"
    void $ defaultSendToNeighbors seqConcurrentlyK (flip sender BroadcastHeader) msg

seqConcurrentlyK :: MonadBaseControl IO m => [KademliaDHT m a] -> KademliaDHT m [a]
seqConcurrentlyK = KademliaDHT . mapConcurrently unKademliaDHT

instance ( MonadDHTDialog s m
         , WithLogger m
         , MonadCatch m
         , MonadIO m
         , MonadMockable m
         , MonadBaseControl IO m
         , Bi DHTData
         , Bi DHTKey
         ) => ___ s (KademliaDHT m) where -- TODO: FIX IT!
    sendToNetwork = sendToNetworkImpl sendH
    sendToNeighbors = defaultSendToNeighbors seqConcurrentlyK sendToNode
    sendToNode addr msg = do
        defaultSendToNode addr msg
        () <$ listenOutbound
      where
        -- [CSL-4][TW-47]: temporary code, to refactor to subscriptions (after TW-47)
        listenOutboundDo = KademliaDHT (asks kdcListenByBinding) >>= ($ AtConnTo addr)
        listenOutbound = listenOutboundDo `catches` [Handler handleAL, Handler handleTE]
        handleAL (AlreadyListeningOutbound _) = return $ pure ()
        handleTE e@(SomeException _) = do
            logDebug $ sformat ("Error listening outbound connection to " %
                                shown % ": " % build) addr e
            return $ pure ()

rejoinNetwork
    :: (MonadIO m, WithLogger m, MonadCatch m, Bi DHTData, Bi DHTKey)
    => KademliaDHT m ()
rejoinNetwork = withDhtLogger $ do
    peers <- getKnownPeers
    logDebug $ sformat ("rejoinNetwork: peers " % build) peers
    when (null peers) $ do
      logWarning "Empty known peer list"
      init <- KademliaDHT $ asks (kdiInitialPeers . kdcDHTInstance_)
      joinNetworkNoThrow init

instance (MonadIO m, MonadCatch m, WithLogger m, Bi DHTData, Bi DHTKey) =>
         MonadDHT (KademliaDHT m) where
    joinNetwork [] = throwM AllPeersUnavailable
    joinNetwork nodes = do
        inst <- KademliaDHT $ asks (kdiHandle . kdcDHTInstance_)
        loggerName <- getLoggerName
        asyncs <-
            mapM
                (liftIO . usingLoggerName loggerName . async . joinNetwork' inst)
                nodes
        waitAnyUnexceptional asyncs >>= handleRes
      where
        handleRes (Just _) = pure ()
        handleRes _        = throwM AllPeersUnavailable
    discoverPeers type_ = do
        inst <- KademliaDHT $ asks (kdiHandle . kdcDHTInstance_)
        _ <- liftIO $ K.lookup inst =<< randomDHTKey type_
        filterByNodeType type_ <$> getKnownPeers
    getKnownPeers = do
        myId <- currentNodeKey
        (inst, initialPeers, explicitInitial) <-
            KademliaDHT $
            (,,) <$> asks kdcDHTInstance_ <*>
            asks (kdiInitialPeers . kdcDHTInstance_) <*>
            asks (kdiExplicitInitial . kdcDHTInstance_)
        extendPeers
          inst
          myId
          (if explicitInitial
            then initialPeers
            else []) =<<
          liftIO (K.dumpPeers $ kdiHandle inst)
      where
        extendPeers inst myId initial peers =
            map snd .
            HM.toList .
            HM.delete myId .
            flip (foldr $ \n -> HM.insert (dhtNodeId n) n) initial .
            HM.fromList . map (\(toDHTNode -> n) -> (dhtNodeId n, n)) <$>
            (updateCache inst =<<
            selectSufficientNodes inst myId peers)

        selectSufficientNodes inst myId l =
            if enchancedMessageBroadcast /= (0 :: Int)
            then concat <$> mapM (getPeersFromBucket enchancedMessageBroadcast inst)
                                 (splitToBuckets (kdiHandle inst) myId l)
            else return l

        bucketIndex origin x = length . takeWhile (not . id) <$> K.distance origin (K.nodeId x)

        insertId origin i hm = do
            bucket <- bucketIndex origin i
            return $ HM.insertWith (++) bucket [i] hm

        splitToBuckets kInst origin peers = flip K.usingKademliaInstance kInst $
            HM.elems <$> foldrM (insertId origin) HM.empty peers

        getPeersFromBucket p inst bucket = do
            cache <- atomically $ readTVar $ kdiKnownPeersCache inst
            let fromCache = tryTake p $ intersect cache bucket
            return $ fromCache ++ tryTake (p - length fromCache) (bucket \\ cache)

        tryTake x l = if length l < x then l else take x l

        updateCache inst peers = do
            atomically $ writeTVar (kdiKnownPeersCache inst) peers
            return peers

    currentNodeKey = KademliaDHT $ asks (kdiKey . kdcDHTInstance_)
    dhtLoggerName _ = "kademlia"

toDHTNode :: K.Node DHTKey -> DHTNode
toDHTNode n = DHTNode (fromKPeer . K.peer $ n) $ K.nodeId n

fromKPeer :: K.Peer -> NetworkAddress
fromKPeer K.Peer{..} = (encodeUtf8 peerHost, fromIntegral peerPort)

toKPeer :: NetworkAddress -> K.Peer
toKPeer (peerHost, peerPort) = K.Peer (decodeUtf8 peerHost) (fromIntegral peerPort)

joinNetwork'
    :: (MonadIO m, MonadThrow m, WithLogger m, Bi DHTKey, Bi DHTData)
    => DHTHandle -> DHTNode -> m ()
joinNetwork' inst node = do
    let node' = K.Node (toKPeer $ dhtAddr node) (dhtNodeId node)
    res <- liftIO $ K.joinNetwork inst node'
    case res of
        K.JoinSuccess -> pure ()
        K.NodeDown -> throwM NodeDown
        K.NodeBanned ->
            logInfo $
            sformat ("joinNetwork: node " % build % " is banned") node
        K.IDClash ->
            logInfo $
            sformat ("joinNetwork: node " % build % " already contains us") node
