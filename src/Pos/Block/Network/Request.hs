{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Wrappers for network requests.

module Pos.Block.Network.Request
       ( mkHeadersRequest
       , replyWithHeadersRequest
       , mkBlocksRequest
       , replyWithBlocksRequest
       , getHeadersOlderExp
       ) where

import           Control.Lens                   (view)
import           Formatting                     (build, sformat, (%))
import           System.Wlog                    (logDebug)
import           Universum

import           Pos.Binary.Block.Network       ()
import           Pos.Block.Network.Server.State (recordBlocksRequest,
                                                 recordHeadersRequest)
import           Pos.Block.Network.Types        (MsgGetBlocks (..), MsgGetHeaders (..))
import           Pos.Communication.Types        (ResponseMode)
import           Pos.Constants                  (k)
import           Pos.Crypto                     (hash)
import           Pos.DB                         (getTip, loadHeadersWhile)
import           Pos.DHT.Model                  (getUserState, replyToNode)
import           Pos.Types                      (HeaderHash, prevBlockL)
import           Pos.WorkMode                   (WorkMode)

-- | Given a starting point hash (we take tip if it's not in storage)
-- it returns not more than 'k' blocks distributed exponentially base
-- 2 relatively to the depth in the blockchain.
getHeadersOlderExp
    :: WorkMode ssc m
    => Maybe (HeaderHash ssc) -> m [HeaderHash ssc]
getHeadersOlderExp upto = do
    tip <- getTip
    let upToReal = fromMaybe tip upto
        whileCond _ depth = depth <= k
    allHeaders <- loadHeadersWhile upToReal whileCond
    let selected = selectIndices (takeHashes allHeaders) twoPowers
    pure selected
  where
    -- Given list of headers newest first, maps it to their hashes
    takeHashes [] = []
    takeHashes headers@(x:_) =
        let prevHashes = map (view prevBlockL) headers
        in hash x : take (length prevHashes - 1) prevHashes
    -- Powers of 2
    twoPowers = (takeWhile (<k) $ 0 : 1 : iterate (*2) 2) ++ [k]
    -- Effectively do @!i@ for any @i@ from the index list applied to
    -- source list. Index list should be inreasing.
    selectIndices :: [a] -> [Int] -> [a]
    selectIndices elems ixs =
        let selGo _ [] _ = []
            selGo [] _ _ = []
            selGo ee@(e:es) ii@(i:is) skipped
                | skipped == i = e : selGo ee is skipped
                | otherwise    = selGo es ii $ succ skipped
        in selGo elems ixs 0

-- | Make 'GetHeaders' message using our main chain. This function
-- chooses appropriate 'from' hashes and puts them into 'GetHeaders'
-- message.
mkHeadersRequest
    :: WorkMode ssc m
    => Maybe (HeaderHash ssc) -> m (MsgGetHeaders ssc)
mkHeadersRequest upto = do
    headers <- getHeadersOlderExp Nothing
    pure $ MsgGetHeaders headers upto

replyWithHeadersRequest
    :: forall ssc m . ResponseMode ssc m
    => Maybe (HeaderHash ssc) -> m ()
replyWithHeadersRequest upto = do
    logDebug "replyWithHeadersRequest: preparing request to be sent"
    msg <- mkHeadersRequest upto
    recordHeadersRequest msg =<< getUserState
    replyToNode msg
    logDebug "replyWithHeadersRequest: data sent"

-- | Make message which requests chain of blocks which is based on our
-- tip. LcaChild is the first block after LCA we don't
-- know. WantedBlock is the newest one we want to get.
mkBlocksRequest :: HeaderHash ssc -> HeaderHash ssc -> MsgGetBlocks ssc
mkBlocksRequest lcaChild wantedBlock =
    MsgGetBlocks
    { mgbFrom = lcaChild
    , mgbTo = wantedBlock
    }

-- | Reply with message which requests chain of blocks which is based
-- on our tip. This request is recorded in SocketState.
replyWithBlocksRequest
    :: forall ssc m . ResponseMode ssc m
    => HeaderHash ssc -> HeaderHash ssc -> m ()
replyWithBlocksRequest lcaChild wantedBlock = do
    logDebug $
        sformat ("replyWithBlocksRequest: asking from (lca child) "%build%" to (new tip) "%build)
                lcaChild wantedBlock
    recordBlocksRequest lcaChild wantedBlock =<< getUserState
    logDebug "replyWithBlocksRequest: replying to node"
    replyToNode msg
    logDebug "replyWithBlocksRequest: replied"
  where
    msg = mkBlocksRequest lcaChild wantedBlock
