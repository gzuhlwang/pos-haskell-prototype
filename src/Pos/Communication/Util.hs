-- | Communication-specific utility functions.

module Pos.Communication.Util
       ( modifyListenerLogger
       ) where

import           System.Wlog        (HasLoggerName, LoggerName, modifyLoggerName)
import           Universum

--import           Pos.DHT.Model    (ListenerDHT (..))
import           Node               (Listener(..))
import           Message.Message    (BinaryP(..))

-- | Append given logger name to the name used by listener.
modifyListenerLogger
    :: (Monad m, HasLoggerName m)
    => LoggerName
    -> Listener BinaryP m
    -> Listener BinaryP m
modifyListenerLogger name listener =
    listener
    --Listener name $ \r -> modifyLoggerName (<> name) (listener r)
