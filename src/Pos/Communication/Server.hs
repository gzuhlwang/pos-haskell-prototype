{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server part.

module Pos.Communication.Server
       ( allListeners
       , forkStrategy
       , serverLoggerName
       , module Pos.Communication.Server.SysStart
       ) where

import           Control.TimeWarp.Rpc                (ForkStrategy (ForkStrategy),
                                                      MessageName)
import           Control.TimeWarp.Timed              (fork_)
import           Data.Tagged                         (untag)
import           System.Wlog                         (LoggerName)
import           Universum

import           Pos.Binary.Communication            ()
import           Pos.Block.Network.Server            (blkForkStrategy, blockListeners)
import           Pos.Communication.Server.Delegation (delegationListeners)
import           Pos.Communication.Server.Protocol   (protocolListeners)
import           Pos.Communication.Server.SysStart
import           Pos.Communication.Types             (MutSocketState)
import           Pos.Communication.Util              (modifyListenerLogger)
import           Pos.DHT.Model                       (ListenerDHT, MonadDHTDialog)
import           Pos.Ssc.Class.Listeners             (SscListenersClass, sscListeners)
import           Pos.Txp.Listeners                   (txListeners)
import           Pos.WorkMode                        (WorkMode)
import           Mockable.Monad                      (MonadMockable)

-- | All listeners running on one node.
allListeners
    :: (SscListenersClass ssc, MonadDHTDialog (MutSocketState ssc) m, WorkMode ssc m)
    => [ListenerDHT (MutSocketState ssc) m]
allListeners =
    map (modifyListenerLogger serverLoggerName) $
    concat
        [ map (modifyListenerLogger "block") blockListeners
        , map (modifyListenerLogger "ssc") $ untag sscListeners
        , map (modifyListenerLogger "tx") txListeners
        , map (modifyListenerLogger "delegation") delegationListeners
        , map (modifyListenerLogger "protocol") protocolListeners
        ]

-- | ForkStrategy of whole server.
forkStrategy
    :: forall ssc.
       Typeable ssc
    => ForkStrategy MessageName
forkStrategy = ForkStrategy forkStrategyImpl
  where
    forkStrategyImpl
        :: forall m. (MonadMockable m)
        => MessageName -> m () -> m ()
    forkStrategyImpl = fromMaybe fork_ . (blkForkStrategy @ssc)

-- | Logger name for server.
serverLoggerName :: LoggerName
serverLoggerName = "server"
