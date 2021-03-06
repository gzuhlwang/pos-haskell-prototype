-- | Utils for common operations with CLI params

module Pos.Launcher.Options
       ( stakesDistr
       ) where

import           Data.Default (def)
import           Universum

import           Pos.Genesis  (StakeDistribution (..))
import           Pos.Types    (mkCoin)

type DistrOption = Maybe (Int, Int)

stakesDistr :: DistrOption -> DistrOption -> StakeDistribution
stakesDistr Nothing Nothing = def
stakesDistr (Just _) (Just _) =
    panic "flat-distr and bitcoin distr are conflicting options"
stakesDistr (Just (nodes, coins)) Nothing =
    FlatStakes (fromIntegral nodes) (mkCoin (fromIntegral coins))
stakesDistr Nothing (Just (nodes, coins)) =
    BitcoinStakes (fromIntegral nodes) (mkCoin (fromIntegral coins))
