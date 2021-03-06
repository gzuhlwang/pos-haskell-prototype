{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

{-| Constants used by algorithm. See paper for more details.
    Some constants are parsed at compile-time (see 'Pos.CompileConfig.Type').
    Others are derived from those.
-}

module Pos.Constants
       (
         -- * Constants mentioned in paper
         k
       , slotDuration
       , epochSlots
       , epochDuration
       , networkDiameter

         -- * SSC constants
       , sharedSeedLength

         -- * Other constants
       , genesisN
       , maxLocalTxs
       , maxBlockProxySKs
       , neighborsSendThreshold
       , RunningMode (..)
       , runningMode
       , isDevelopment
       , defaultPeers
       , sysTimeBroadcastSlots
       , mpcSendInterval
       , vssMaxTTL
       , protocolMagic
       , enchancedMessageBroadcast

         -- * Malicious activity detection constants
       , mdNoBlocksSlotThreshold
       , mdNoCommitmentsEpochThreshold

         -- * Update system constants
       , curProtocolVersion
       , curSoftwareVersion
       , appSystemTag
       , updateServers
       , updateProposalThreshold
       , updateVoteThreshold
       , updateImplicitApproval
       ) where

import           Control.TimeWarp.Timed     (Microsecond, sec)
import           Data.String                (String)
import           Language.Haskell.TH.Syntax (lift, runIO)
import           System.Environment         (lookupEnv)
import qualified Text.Parsec                as P
import           Universum                  hiding (lift)

import           Pos.CLI                    (dhtNodeParser)
import           Pos.CompileConfig          (CompileConfig (..), compileConfig)
import           Pos.DHT.Model.Types        (DHTNode)
import           Pos.Types.Timestamp        (Timestamp)
import           Pos.Types.Types            (CoinPortion, unsafeCoinPortion)
import           Pos.Types.Version          (ApplicationName, ProtocolVersion (..),
                                             SoftwareVersion (..), mkApplicationName)
import           Pos.Update.Types           (SystemTag, mkSystemTag)
import           Pos.Util                   ()

----------------------------------------------------------------------------
-- Main constants mentioned in paper
----------------------------------------------------------------------------

-- | Consensus guarantee (i.e. after what amount of blocks can we consider
-- blocks stable?).
k :: Integral a => a
k = fromIntegral . ccK $ compileConfig

-- | Length of slot. Also see 'Pos.CompileConfig.ccSlotDurationSec'.
slotDuration :: Microsecond
slotDuration = sec . ccSlotDurationSec $ compileConfig

-- | Number of slots inside one epoch.
epochSlots :: Integral a => a
epochSlots = 6 * k

-- | Length of one epoch in 'Microsecond's.
epochDuration :: Microsecond
epochDuration = epochSlots * slotDuration

-- | Estimated time needed to broadcast message from one node to all
-- other nodes. Also see 'Pos.CompileConfig.ccNetworkDiameter'.
networkDiameter :: Microsecond
networkDiameter = sec . ccNetworkDiameter $ compileConfig

----------------------------------------------------------------------------
-- SSC
----------------------------------------------------------------------------

-- | Length of shared seed.
sharedSeedLength :: Integral a => a
sharedSeedLength = 32

-- | Length of interval during which node should send her MPC
-- message. Relevant only for one SSC implementation.
-- Also see 'Pos.CompileConfig.ccMpcSendInterval'.
mpcSendInterval :: Microsecond
mpcSendInterval = sec . fromIntegral . ccMpcSendInterval $ compileConfig

----------------------------------------------------------------------------
-- Other constants
----------------------------------------------------------------------------

-- | See 'Pos.CompileConfig.ccGenesisN'.
genesisN :: Integral i => i
genesisN = fromIntegral . ccGenesisN $ compileConfig

-- | Maximum amount of transactions we have in storage
-- (i.e. we can accept without putting them in block).
-- There're next kind of storages in our implementation:
--
-- * temporary storage of transactions
--
-- * utxo map that corresponds to it
--
-- * utxo of blocks in history
--
-- This constant is size of first set.
-- Also see 'Pos.CompileConfig.ccMaxLocalTxs'.
maxLocalTxs :: Integral i => i
maxLocalTxs = fromIntegral . ccMaxLocalTxs $ compileConfig

-- | Maximum number of PSKs allowed in block
maxBlockProxySKs :: Integral i => i
maxBlockProxySKs = fromIntegral . ccMaxBlockProxySKs $ compileConfig

-- | /Time-lord/ node announces system start time by broadcast. She does it
-- during first 'Pos.CompileConfig.ccSysTimeBroadcastSlots' slots.
sysTimeBroadcastSlots :: Integral i => i
sysTimeBroadcastSlots = fromIntegral . ccSysTimeBroadcastSlots $ compileConfig

-- | See 'Pos.CompileConfig.ccNeighboursSendThreshold'.
neighborsSendThreshold :: Integral a => a
neighborsSendThreshold =
    fromIntegral . ccNeighboursSendThreshold $ compileConfig

-- | Defines mode of running application: in tested mode or in production.
data RunningMode
    = Development
    | Production { rmSystemStart :: !Timestamp}

-- | Current running mode.
runningMode :: RunningMode
#ifdef DEV_MODE
runningMode = Development
#else
runningMode = Production $ panic "System start is not known!"
#endif

-- | @True@ if current mode is 'Development'.
isDevelopment :: Bool
isDevelopment = case runningMode of
                  Development -> True
                  _           -> False

-- | See 'Pos.CompileConfig.ccDefaultPeers'.
defaultPeers :: [DHTNode]
defaultPeers = map parsePeer . ccDefaultPeers $ compileConfig
  where
    parsePeer :: [Char] -> DHTNode
    parsePeer =
        either (panic . show) identity .
        P.parse dhtNodeParser "Compile time config"

-- | Max VSS certificate TTL (Ssc.GodTossing part)
vssMaxTTL :: Integral i => i
vssMaxTTL = fromIntegral . ccVssMaxTTL $ compileConfig

-- | Protocol magic constant. Is put to block serialized version to
-- distinguish testnet and realnet (for example, possible usages are
-- wider).
protocolMagic :: Int32
protocolMagic = fromIntegral . ccProtocolMagic $ compileConfig

-- | Setting this to true enables enchanced message broadcast
enchancedMessageBroadcast :: Integral a => a
enchancedMessageBroadcast = fromIntegral $ ccEnchancedMessageBroadcast compileConfig

----------------------------------------------------------------------------
-- Malicious activity
----------------------------------------------------------------------------

-- | Number of slots used by malicious actions detection to check if
-- we are not receiving generated blocks.
mdNoBlocksSlotThreshold :: Integral i => i
mdNoBlocksSlotThreshold = fromIntegral . ccMdNoBlocksSlotThreshold $ compileConfig

-- | Number of epochs used by malicious actions detection to check if
-- our commitments are not included in blockchain.
mdNoCommitmentsEpochThreshold :: Integral i => i
mdNoCommitmentsEpochThreshold = fromIntegral . ccMdNoCommitmentsEpochThreshold $ compileConfig

----------------------------------------------------------------------------
-- Update system
----------------------------------------------------------------------------

cardanoSlAppName :: ApplicationName
cardanoSlAppName = either (panic . (<>) "Failed to init cardanoSlAppName: ")
                      identity $ mkApplicationName "cardano"

appSystemTag :: SystemTag
appSystemTag = $(do
    mbTag <- runIO (lookupEnv "CSL_SYSTEM_TAG")
    case mbTag of
        Nothing ->
#ifdef DEV_MODE
            [|panic "'appSystemTag' can't be used if \
                    \env var \"CSL_SYSTEM_TAG\" wasn't set \
                    \during compilation" |]
#else
            fail "Failed to init appSystemTag: \
                 \couldn't find env var \"CSL_SYSTEM_TAG\""
#endif
        Just tag -> lift =<< mkSystemTag (toText tag))

-- | Protocol version application uses
curProtocolVersion :: ProtocolVersion
curProtocolVersion = ProtocolVersion 0 0 0

-- | Version of application (code running)
curSoftwareVersion :: SoftwareVersion
curSoftwareVersion = SoftwareVersion cardanoSlAppName 0

-- | Update servers
updateServers :: [String]
updateServers = ccUpdateServers compileConfig

-- | Portion of total stake such that block containing
-- UpdateProposal must contain positive votes for this proposal
-- from stakeholders owning at least this amount of stake.
updateProposalThreshold :: CoinPortion
updateProposalThreshold = unsafeCoinPortion $ ccUpdateProposalThreshold compileConfig

-- GHC stage restriction
-- staticAssert
--     (getCoinPortion updateProposalThreshold >= 0)
--     "updateProposalThreshold is negative"

-- staticAssert
--     (getCoinPortion updateProposalThreshold <= 1)
--     "updateProposalThreshold is more than 1"

-- | Portion of total stake necessary to vote for or against update.
updateVoteThreshold :: CoinPortion
updateVoteThreshold = unsafeCoinPortion $ ccUpdateVoteThreshold compileConfig

-- GHC stage restriction
-- staticAssert
--     (getCoinPortion updateVoteThreshold >= 0)
--     "updateVoteThreshold is negative"

-- staticAssert
--     (getCoinPortion updateVoteThreshold <= 1)
--     "updateVoteThreshold is more than 1"

-- | Number of slots after which update is implicitly approved
-- unless it has more negative votes than positive.
updateImplicitApproval :: Integral i => i
updateImplicitApproval = fromIntegral $ ccUpdateImplicitApproval compileConfig
