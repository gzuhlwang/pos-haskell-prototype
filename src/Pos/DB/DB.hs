-- | Higher-level DB functionality.

module Pos.DB.DB
       ( openNodeDBs
       , getTipBlock
       , getTipBlockHeader
       , loadBlocksFromTipWhile
       ) where

import           Control.Monad.Trans.Resource (MonadResource)
import           System.Directory             (createDirectoryIfMissing,
                                               doesDirectoryExist,
                                               removeDirectoryRecursive)
import           System.FilePath              ((</>))
import           Universum

import           Pos.DB.Block                 (getBlock, loadBlocksWithUndoWhile,
                                               prepareBlockDB)
import           Pos.DB.Class                 (MonadDB)
import           Pos.DB.Error                 (DBError (DBMalformed))
import           Pos.DB.Functions             (openDB)
import           Pos.DB.GState                (getTip, prepareGStateDB)
import           Pos.DB.Holder                (runDBHolder)
import           Pos.DB.Misc                  (prepareMiscDB)
import           Pos.DB.Types                 (NodeDBs (..))
import           Pos.Eligibility              (findRichmenPure)
import           Pos.Genesis                  (genesisLeaders)
import           Pos.Ssc.Class.Types          (Ssc)
import           Pos.Types                    (Block, BlockHeader, Undo, Utxo,
                                               getBlockHeader, headerHash, mkCoin,
                                               mkGenesisBlock)

-- | Open all DBs stored on disk.
openNodeDBs
    :: (Ssc ssc, MonadResource m)
    => Bool -> FilePath -> Utxo -> m (NodeDBs ssc)
openNodeDBs recreate fp customUtxo = do
    liftIO $
        whenM ((recreate &&) <$> doesDirectoryExist fp) $
            removeDirectoryRecursive fp
    let blockPath = fp </> "blocks"
    let utxoPath = fp </> "utxo"
    let miscPath = fp </> "misc"
    mapM_ ensureDirectoryExists [blockPath, utxoPath, miscPath]
    res <- NodeDBs <$> openDB blockPath <*> openDB utxoPath <*> openDB miscPath
    let prepare = do
          prepareBlockDB genesisBlock0
          prepareGStateDB customUtxo initialTip
          prepareMiscDB leaders0 richmen0
    res <$ runDBHolder res prepare
  where
    leaders0 = genesisLeaders customUtxo
    -- [CSL-93] Use eligibility threshold here
    richmen0 = findRichmenPure customUtxo (mkCoin 0)
    ensureDirectoryExists
        :: MonadIO m
        => FilePath -> m ()
    ensureDirectoryExists = liftIO . createDirectoryIfMissing True
    genesisBlock0 = mkGenesisBlock Nothing 0 leaders0
    initialTip = headerHash genesisBlock0

-- | Get block corresponding to tip.
getTipBlock
    :: (Ssc ssc, MonadDB ssc m)
    => m (Block ssc)
getTipBlock = maybe onFailure pure =<< getBlock =<< getTip
  where
    onFailure = throwM $ DBMalformed "there is no block corresponding to tip"

-- | Get BlockHeader corresponding to tip.
getTipBlockHeader
    :: (Ssc ssc, MonadDB ssc m)
    => m (BlockHeader ssc)
getTipBlockHeader = getBlockHeader <$> getTipBlock

-- | Load blocks from BlockDB starting from tip and while @condition@ is true.
-- The head of returned list is the youngest block.
loadBlocksFromTipWhile
    :: (Ssc ssc, MonadDB ssc m)
    => (Block ssc -> Int -> Bool) -> m [(Block ssc, Undo)]
loadBlocksFromTipWhile condition = getTip >>= loadBlocksWithUndoWhile condition
