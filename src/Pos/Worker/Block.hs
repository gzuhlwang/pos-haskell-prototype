-- | Block processing related workers.

module Pos.Worker.Block
       ( blkOnNewSlot
       , blkWorkers
       ) where

import           Control.Lens              (ix, (^.), (^?))
import           Control.TimeWarp.Logging  (logInfo, logWarning)
import           Control.TimeWarp.Timed    (Microsecond, for, minute, repeatForever, wait)
import           Formatting                (build, sformat, (%))
import           Serokell.Util.Exceptions  ()
import           Universum

import           Pos.Communication.Methods (announceBlock)
import           Pos.Constants             (networkDiameter, slotDuration)
import           Pos.State                 (getHeadBlock, getLeaders)
import           Pos.Types                 (SlotId (..), gbHeader, slotIdF)
import           Pos.WorkMode              (WorkMode, getNodeContext, ncPublicKey)

-- | Action which should be done when new slot starts.
blkOnNewSlot :: WorkMode m => SlotId -> m ()
blkOnNewSlot slotId@SlotId {..} = do
    leaders <- getLeaders siEpoch
    ourPk <- ncPublicKey <$> getNodeContext
    let leader = leaders ^? ix (fromIntegral siSlot)
    when (leader == Just ourPk) $ onNewSlotWhenLeader slotId

onNewSlotWhenLeader :: WorkMode m => SlotId -> m ()
onNewSlotWhenLeader slotId = do
    logInfo $
        sformat ("I am leader of "%slotIdF%", I will create block soon") slotId
    wait $ for (slotDuration - networkDiameter)
    logInfo "It's time to create a block for current slot"
    announceBlock undefined
    -- TODO

-- | All workers specific to block processing.
-- Exceptions:
-- 1. Worker which ticks when new slot starts.
blkWorkers :: WorkMode m => [m ()]
blkWorkers = [blocksTransmitter]

blocksTransmitterInterval :: Microsecond
blocksTransmitterInterval = minute 1

blocksTransmitter :: WorkMode m => m ()
blocksTransmitter =
    repeatForever blocksTransmitterInterval onError $
    do headBlock <- getHeadBlock
       case headBlock of
           Left _ -> logWarning "Head block is genesis for some reason, omgwtf!"
           Right mainBlock -> announceBlock (mainBlock ^. gbHeader)
  where
    onError e =
        blocksTransmitterInterval <$
        logWarning (sformat ("Error occured in blocksTransmitter: " %build) e)