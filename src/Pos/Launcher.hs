{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Launcher of full node or simple operations.

module Pos.Launcher
       ( LoggingParams (..)
       , NodeParams (..)
       , BaseParams (..)
       , getCurTimestamp
       , runNode
       , runNodeReal
       , runNodeBench
       , submitTx
       , submitTxReal
       , runSupporterReal
       , runTimeSlaveReal
       , runTimeLordReal
       ) where

import           Control.Concurrent.MVar  (newEmptyMVar, takeMVar)
import           Control.TimeWarp.Logging (LoggerName, Severity (Warning),
                                           WithNamedLogger, initLogging, logError,
                                           logInfo, logWarning, setSeverity,
                                           setSeverityMaybe, usingLoggerName)
import           Control.TimeWarp.Rpc     (BinaryP (..), Dialog, MonadDialog,
                                           NetworkAddress, Transfer, runDialog,
                                           runTransfer)
import           Control.TimeWarp.Timed   (MonadTimed, currentTime, fork, killThread,
                                           repeatForever, runTimedIO, sec, sleepForever)
import           Data.Default             (Default (def))
import           Formatting               (build, sformat, (%))
import           Universum                hiding (catch, killThread)

import           Pos.Communication        (SysStartRequest (..), allListeners, sendTx,
                                           sysStartMessageNames, sysStartReqListener,
                                           sysStartRespListener)
import           Pos.Constants            (RunningMode (..), isDevelopment, runningMode)
import           Pos.Crypto               (SecretKey, VssKeyPair, hash, sign)
import           Pos.DHT                  (DHTKey, DHTNode (dhtAddr), DHTNodeType (..),
                                           ListenerDHT, MonadDHT (..), filterByNodeType,
                                           mapListenerDHT, sendToNeighbors)
import           Pos.DHT.Real             (KademliaDHT, KademliaDHTConfig (..),
                                           runKademliaDHT)
import           Pos.State                (NodeState, openMemState, openState)
import           Pos.Types                (Address, Coin, Timestamp (Timestamp), Tx (..),
                                           TxId, TxIn (..), TxOut (..), timestampF, txF)
import           Pos.Worker               (runWorkers)
import           Pos.WorkMode             (BenchmarkT (..), ContextHolder (..),
                                           DBHolder (..), NoBenchmarkT (..),
                                           NodeContext (..), RealMode, SupportMode,
                                           WorkMode, getNodeContext, ncSecretKey)

-- | Get current time as Timestamp. It is intended to be used when you
-- launch the first node. It doesn't make sense in emulation mode.
getCurTimestamp :: IO Timestamp
getCurTimestamp = Timestamp <$> runTimedIO currentTime

-- | Run full node in any WorkMode.
runNode :: WorkMode m => m ()
runNode = do
    peers <- discoverPeers DHTFull
    logInfo $ sformat ("Known peers: " % build) peers

    runWorkers
    sleepForever

-- | Construct Tx with a single input and single output and send it to
-- the given network addresses.
submitTx :: WorkMode m => [NetworkAddress] -> (TxId, Word32) -> (Address, Coin) -> m ()
submitTx na (txInHash, txInIndex) (txOutAddress, txOutValue) =
    if null na
      then logError "No addresses to send"
      else do
        sk <- ncSecretKey <$> getNodeContext
        let txOuts = [TxOut {..}]
            txIns = [TxIn {txInSig = sign sk (txInHash, txInIndex, txOuts), ..}]
            tx = Tx {txInputs = txIns, txOutputs = txOuts}
            txId = hash tx
        logInfo $ sformat ("Submitting transaction: "%txF) tx
        logInfo $ sformat ("Transaction id: "%build) txId
        mapM_ (`sendTx` tx) na

-- | Submit tx in real mode.
submitTxReal :: NodeParams
             -> (TxId, Word32)
             -> (Address, Coin)
             -> IO ()
submitTxReal np input addrCoin = runRealMode np [] $ do
    peers <- getKnownPeers
    let na = dhtAddr <$> filterByNodeType DHTFull peers
    getNoBenchmarkT $ submitTx na input addrCoin

----------------------------------------------------------------------------
-- Parameters
----------------------------------------------------------------------------

data LoggingParams = LoggingParams
    { lpRootLogger     :: !LoggerName
    , lpMainSeverity   :: !Severity
    , lpDhtSeverity    :: !(Maybe Severity)
    , lpServerSeverity :: !(Maybe Severity)
    , lpCommSeverity   :: !(Maybe Severity)
    , lpWorkerSeverity :: !(Maybe Severity)
    } deriving (Show)

instance Default LoggingParams where
    def =
        LoggingParams
        { lpRootLogger     = mempty
        , lpMainSeverity   = Warning
        , lpDhtSeverity    = Nothing
        , lpServerSeverity = Nothing
        , lpCommSeverity   = Nothing
        , lpWorkerSeverity = Nothing
        }

-- | Parameters necessary to run node.
data NodeParams = NodeParams
    { npDbPath      :: !(Maybe FilePath)
    , npRebuildDb   :: !Bool
    , npSecretKey   :: !SecretKey
    , npVssKeyPair  :: !VssKeyPair
    , npBaseParams  :: !BaseParams
    , npSystemStart :: !Timestamp
    } deriving (Show)

data BaseParams = BaseParams
    { bpLogging      :: !LoggingParams
    , bpPort         :: !Word16
    , bpDHTPeers     :: ![DHTNode]
    , bpDHTKeyOrType :: !(Either DHTKey DHTNodeType)
    } deriving (Show)

----------------------------------------------------------------------------
-- Service node runners
----------------------------------------------------------------------------

runTimeSlaveReal :: BaseParams -> IO Timestamp
runTimeSlaveReal bp = do
    mvar <- liftIO newEmptyMVar
    runServiceMode bp (listeners mvar) $
      case runningMode of
         Development -> do
           tId <- fork $
             repeatForever (sec 5) (const . return $ sec 5) $ do
               logInfo "Asking neighbors for system start"
               void $ sendToNeighbors SysStartRequest
           t <- liftIO $ takeMVar mvar
           killThread tId
           t <$ logInfo (sformat ("[Time slave] adopted system start " % timestampF) t)
         Production ts -> logWarning "Time slave launched in Production" $> ts
  where
    listeners mvar =
      if isDevelopment
         then [sysStartReqListener Nothing, sysStartRespListener mvar]
         else []

runTimeLordReal :: LoggingParams -> IO Timestamp
runTimeLordReal lp = do
    setupLoggingReal lp
    runTimed (lpRootLogger lp) $ do
        t <- Timestamp <$> currentTime
        t <$ logInfo (sformat ("[Time lord] System start: " %timestampF) t)

runSupporterReal :: BaseParams -> IO ()
runSupporterReal bp = runServiceMode bp [] $ do
    supporterKey <- currentNodeKey
    logInfo $ sformat ("Supporter key: " % build) supporterKey
    repeatForever (sec 5) (const . return $ sec 5) $
        getKnownPeers >>= logInfo . sformat ("Known peers: " % build)

-----------------------------------------------------------------------------
-- Main launchers
-----------------------------------------------------------------------------

-- | Run full node in real mode.
runNodeReal :: NodeParams -> IO ()
runNodeReal np@NodeParams {..} = runRealMode np listeners $ getNoBenchmarkT runNode
  where
    listeners = if isDevelopment
                then sysStartReqListener (Just npSystemStart) : noBenchListeners
                else noBenchListeners
    noBenchListeners = map (mapListenerDHT getNoBenchmarkT) allListeners

-- | Run full node in benchmarking node
-- TODO: spawn here additional listener, which would accept stat queries
runNodeBench :: NodeParams -> IO ()
runNodeBench np = runRealMode np benchListeners $ getBenchmarkT runNode
  where benchListeners = map (mapListenerDHT getBenchmarkT) allListeners


----------------------------------------------------------------------------
-- Real mode runners
----------------------------------------------------------------------------

-- TODO: use bracket
runRealMode :: NodeParams -> [ListenerDHT RealMode] -> RealMode a -> IO a
runRealMode NodeParams {..} listeners action = do
    setupLoggingReal logParams
    db <- openDb
    runTimed loggerName . runDBH db . runCH . runKDHT npBaseParams listeners $
        nodeStartMsg npBaseParams >> action
  where
    logParams  = bpLogging npBaseParams
    loggerName = lpRootLogger logParams

    openDb :: IO NodeState
    openDb = runTimed loggerName . runCH $
         maybe openMemState (openState npRebuildDb) npDbPath

    runDBH :: NodeState -> DBHolder m a -> m a
    runDBH db = flip runReaderT db . getDBHolder

    runCH :: ContextHolder m a -> m a
    runCH = flip runReaderT ctx . getContextHolder
      where
        ctx = NodeContext
              { ncSystemStart = npSystemStart
              , ncSecretKey = npSecretKey
              , ncVssKeyPair = npVssKeyPair
              }

runServiceMode :: BaseParams -> [ListenerDHT SupportMode] -> SupportMode a -> IO a
runServiceMode bp@BaseParams {..} listeners action = do
    setupLoggingReal bpLogging
    runTimed (lpRootLogger bpLogging) . runKDHT bp listeners $
        nodeStartMsg bp >> action

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

runKDHT :: (WithNamedLogger m, MonadIO m, MonadTimed m, MonadMask m, MonadDialog BinaryP m)
        => BaseParams -> [ListenerDHT (KademliaDHT m)] -> KademliaDHT m a -> m a
runKDHT BaseParams {..} listeners = runKademliaDHT kadConfig
  where
    kadConfig =
      KademliaDHTConfig
      { kdcKeyOrType = bpDHTKeyOrType
      , kdcPort = bpPort
      , kdcListeners = listeners
      , kdcMessageCacheSize = 1000000
      , kdcEnableBroadcast = False
      , kdcInitialPeers = bpDHTPeers
      , kdcNoCacheMessageNames = if isDevelopment
                                 then sysStartMessageNames
                                    else []
      }

runTimed :: LoggerName -> Dialog BinaryP Transfer a -> IO a
runTimed loggerName =
    runTimedIO .
    usingLoggerName loggerName . runTransfer . runDialog BinaryP

setupLoggingReal :: LoggingParams -> IO ()
setupLoggingReal LoggingParams {..} = do
    initLogging Warning
    setSeverity lpRootLogger lpMainSeverity
    setSeverityMaybe
        (lpRootLogger <> dhtLoggerName (Proxy :: Proxy RealMode))
        lpDhtSeverity
    -- TODO: `comm` shouldn't be hardcoded, it should be taken
    -- from MonadTransfer or something
    setSeverityMaybe (lpRootLogger <> "comm") lpCommSeverity

nodeStartMsg :: (WithNamedLogger m, MonadIO m) => BaseParams -> m ()
nodeStartMsg BaseParams {..} = logInfo msg
  where msg = sformat ("Started node, joining to DHT network "%build) bpDHTPeers
