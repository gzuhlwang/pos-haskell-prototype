-- | High-level scenarios which can be launched.

module Pos.Launcher.Scenario
       ( runNode
       , initSemaphore
       , initLeaders
       ) where

import           Control.Concurrent.MVar (putMVar)
import           Control.TimeWarp.Timed  (currentTime, for, fork, sleepForever, wait)
import           Formatting              (build, sformat, (%))
import           System.Wlog             (logError, logInfo)
import           Universum

import           Pos.Constants           (k)
import           Pos.Context             (NodeContext (..), getNodeContext,
                                          ncPubKeyAddress, ncPublicKey, writeLeaders)
import qualified Pos.DB                  as DB
import           Pos.DHT.Model           (DHTNodeType (DHTFull), discoverPeers)
import           Pos.Slotting            (getCurrentSlot)
import           Pos.Ssc.Class           (SscConstraint)
import           Pos.Types               (SlotId (..), Timestamp (Timestamp), addressHash)
import           Pos.Util                (inAssertMode)
import           Pos.Worker              (runWorkers)
import           Pos.WorkMode            (WorkMode)

-- | Run full node in any WorkMode.
runNode :: (SscConstraint ssc, WorkMode ssc m) => [m ()] -> m ()
runNode plugins = do
    inAssertMode $ logInfo "Assert mode on"
    pk <- ncPublicKey <$> getNodeContext
    addr <- ncPubKeyAddress <$> getNodeContext
    let pkHash = addressHash pk
    logInfo $ sformat ("My public key is: "%build%
                       ", address: "%build%
                       ", pk hash: "%build) pk addr pkHash
    peers <- discoverPeers DHTFull
    logInfo $ sformat ("Known peers: " % build) peers

    initSemaphore
    initLeaders
    waitSystemStart
    runWorkers
    mapM_ fork plugins
    sleepForever

-- Sanity check in case start time is in future (may happen if clocks
-- are not accurately synchronized, for example).
waitSystemStart :: WorkMode ssc m => m ()
waitSystemStart = do
    Timestamp start <- ncSystemStart <$> getNodeContext
    cur <- currentTime
    when (cur < start) $ wait (for (start - cur))

initSemaphore :: (WorkMode ssc m) => m ()
initSemaphore = do
    semaphore <- ncBlkSemaphore <$> getNodeContext
    unlessM
        (liftIO $ isEmptyMVar semaphore)
        (logError "ncBlkSemaphore is not empty at the very beginning")
    tip <- DB.getTip
    liftIO $ putMVar semaphore tip

initLeaders :: WorkMode ssc m => m ()
initLeaders = do
    (epochIndex, leaders) <- DB.getLeaders
    SlotId {..} <- getCurrentSlot
    when (siSlot < k && siEpoch == epochIndex) $ writeLeaders epochIndex leaders
