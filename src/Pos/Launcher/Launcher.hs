{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Applications of runners to scenarios.

module Pos.Launcher.Launcher
       (
         -- * Node launchers.
         NodeRunner
       , NodeRunnerClass (runNodeIO)
       , runNodeProduction
       , runNodeStats

         -- * Utility launchers.
       ) where

import           Universum

import           Mockable.Production   (Production, runProduction)
import           Mockable.Concurrent   (wait)
import           Control.TimeWarp.Timed(for, Microsecond)
import           Control.Monad.IO.Class(liftIO)

import           Pos.DHT.Real          (KademliaDHTInstance)
import           Pos.Launcher.Param    (NodeParams (..))
import           Pos.Launcher.Runner   (runProductionMode, runStatsMode)
import           Pos.Launcher.Scenario (runNode)
import           Pos.Ssc.Class         (SscConstraint)
import           Pos.Ssc.Class.Types   (SscParams)
import           Pos.WorkMode          (ProductionMode, StatsMode)

-----------------------------------------------------------------------------
-- Main launchers
-----------------------------------------------------------------------------

-- Too hard :(
type NodeRunner m = KademliaDHTInstance -> [m ()] -> NodeParams -> Production ()

class NodeRunnerClass ssc m where
    runNodeIO :: KademliaDHTInstance -> [m ()] -> NodeParams -> SscParams ssc -> Production ()

-- | Run full node in real mode.
runNodeProduction
    :: forall ssc.
       SscConstraint ssc
    => KademliaDHTInstance -> [ProductionMode ssc ()] -> NodeParams -> SscParams ssc -> Production ()
runNodeProduction inst plugins np sscnp = runProductionMode inst np sscnp (runNode @ssc plugins)

instance SscConstraint ssc => NodeRunnerClass ssc (ProductionMode ssc) where
    runNodeIO a b c d = runProduction $ runNodeProduction a b c d

-- | Run full node in benchmarking node
runNodeStats
    :: forall ssc.
       SscConstraint ssc
    => KademliaDHTInstance -> [StatsMode ssc ()] -> NodeParams -> SscParams ssc -> Production ()
runNodeStats inst plugins np sscnp = runStatsMode inst np sscnp (runNode @ssc plugins)

instance SscConstraint ssc => NodeRunnerClass ssc (StatsMode ssc) where
    runNodeIO a b c d = runProduction $ runNodeStats a b c d

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------
