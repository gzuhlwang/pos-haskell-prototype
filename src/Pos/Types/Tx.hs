{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Transaction related functions.

module Pos.Types.Tx
       ( verifyTxAlone
       , VTxGlobalContext (..)
       , VTxLocalContext (..)
       , verifyTx
       , verifyTxPure
       , topsortTxs
       ) where

import           Control.Lens         (makeLenses, use, uses, (%=), (.=), (^.))
import           Control.Monad.Except (runExceptT)
import           Data.Bifunctor       (first)
import qualified Data.HashMap.Strict  as HM
import qualified Data.HashSet         as HS
import           Data.List            (tail, zipWith3)
import           Formatting           (build, int, sformat, (%))
import           Serokell.Util        (VerificationRes, formatAllErrors,
                                       verResToMonadError, verifyGeneric)
import           Universum

import           Pos.Binary.Types     ()
import           Pos.Crypto           (Hash, WithHash (..), checkSig, hash)
import           Pos.Script           (Script (..), isKnownScriptVersion, txScriptCheck)
import           Pos.Types.Address    (addressDetailedF)
import           Pos.Types.Coin       (coinToInteger, sumCoins)
import           Pos.Types.Types      (Address (..), StakeholderId, Tx (..), TxAux,
                                       TxDistribution (..), TxIn (..), TxInWitness (..),
                                       TxOut (..), TxOutAux, checkPubKeyAddress,
                                       checkScriptAddress, coinF, mkCoin)
import           Pos.Util             (allDistinct)

----------------------------------------------------------------------------
-- Verification
----------------------------------------------------------------------------

-- | Verify that Tx itself is correct. Most likely you will also want
-- to verify that inputs are legal, signed properly and have enough coins;
-- 'verifyTxAlone' doesn't do that.
verifyTxAlone :: Tx -> VerificationRes
verifyTxAlone Tx {..} =
    mconcat
        [ verifyGeneric
              [ (not (null txInputs), "transaction doesn't have inputs")
              , (not (null txOutputs), "transaction doesn't have outputs")
              ]
        , verifyOutputs
        ]
  where
    verifyOutputs = verifyGeneric $ concat $
                    zipWith outputPredicates [0..] txOutputs
    outputPredicates (i :: Word) TxOut{..} = [
      ( txOutValue > mkCoin 0
      , sformat ("output #"%int%" has non-positive value: "%coinF)
                i txOutValue) ]

-- CSL-366 Add context-dependent variables to scripts
-- Postponed for now, should be done in near future.
-- Maybe these datatypes should be moved to Types.
-- | Global context data needed for script execution -- is same for
-- the whole transaction. VT stands for "Verify Tx". Currently empty.
data VTxGlobalContext = VTxGlobalContext
    {
--      vtgSlotId   :: SlotId                  -- ^ Slot id of block transaction is checked in
--    , vtgLeaderId :: AddressHash (PublicKey) -- ^ Leader id of block transaction is checked in
    } deriving (Show)

-- | Local context data for scripts -- differs per input.
data VTxLocalContext = VTxLocalContext
    {
--      vtlSlotId :: SlotId -- ^ Slot of the block transaction output was declared in
     vtlTxOut  :: TxOutAux  -- ^ Transaction output
    } deriving (Show)

-- | CHECK: Verify Tx correctness using magic function which resolves
-- input into Address and Coin. It optionally does checks from
-- 'verifyTxAlone' and also the following checks:
--
-- * sum of inputs >= sum of outputs;
-- * every input has a proper witness verifying that input;
-- * script witnesses have matching script versions;
-- * every input is a known unspent output.
--
-- Note that 'verifyTx' doesn't attempt to verify scripts with versions
-- higher than maximum script version we can handle. That's because we want
-- blocks with such transactions to be accepted (to avoid hard
-- forks). However, we won't include such transactions into blocks when we're
-- creating a block.
verifyTx
    :: (Monad m)
    => Bool
    -> VTxGlobalContext
    -> (TxIn -> m (Maybe VTxLocalContext))
    -> TxAux
    -> m (Either Text [TxOutAux])
verifyTx verifyAlone gContext inputResolver txs@(Tx {..}, _, _) = do
    extendedInputs <- mapM extendInput txInputs
    runExceptT $ do
        verResToMonadError formatAllErrors $
            verifyTxDo verifyAlone gContext extendedInputs txs
        return $ map (vtlTxOut . snd) . catMaybes $ extendedInputs
  where
    extendInput txIn = fmap (txIn, ) <$> inputResolver txIn

verifyTxDo :: Bool
           -> VTxGlobalContext
           -> [Maybe (TxIn, VTxLocalContext)]
           -> TxAux
           -> VerificationRes
verifyTxDo verifyAlone _gContext extendedInputs (tx@Tx{..}, witnesses, distrs) =
    mconcat [verifyAloneRes, verifyCounts, verifySum, verifyInputs,
             verifyDistributions]
  where
    verifyAloneRes | verifyAlone = verifyTxAlone tx
                   | otherwise = mempty
    outSum :: Integer
    outSum = sumCoins $ map txOutValue txOutputs
    resolvedInputs = catMaybes extendedInputs
    inpSum :: Integer
    inpSum = sumCoins $ map (txOutValue . fst . vtlTxOut . snd) resolvedInputs
    txOutHash = hash txOutputs
    distrsHash = hash distrs
    verifyCounts =
        verifyGeneric
            [ ( length txInputs == length witnesses
              , sformat ("length of inputs != length of witnesses "%
                         "("%int%" != "%int%")")
                  (length txInputs) (length witnesses) )
            ]
    verifyDistributions =
        verifyGeneric $
            [ ( length txOutputs == length (getTxDistribution distrs)
              , "length of outputs != length of tx distribution")
            ]
            ++
            do (i, (TxOut{..}, d)) <-
                   zip [0 :: Int ..] (zip txOutputs (getTxDistribution distrs))
               case txOutAddress of
                   PubKeyAddress _ ->
                       [ ( null d
                         , sformat ("output #"%int%" with pubkey address "%
                                    "has non-empty distribution") i)
                       ]
                   ScriptAddress _ ->
                       let sumDist = sumCoins (map snd d)
                       in [ (sumDist <= coinToInteger txOutValue,
                             sformat ("output #"%int%" has distribution "%
                                      "sum("%int%") > txOutValue("%coinF%")")
                                     i sumDist txOutValue)
                          , (allDistinct (map fst d :: [StakeholderId]),
                             sformat ("output #"%int%"'s distribution "%
                                      "has duplicated addresses")
                                     i)
                          , (all (> mkCoin 0) (map snd d),
                             sformat ("output #"%int%"'s distribution "%
                                      "assigns 0 coins to some addresses")
                                     i)
                          ]
    verifySum =
        let resInps = length resolvedInputs
            extInps = length extendedInputs
            allInputsExist = resInps == extInps
            verifier
                | allInputsExist =
                    ( inpSum >= outSum
                    , sformat ("sum of outputs is more than sum of inputs ("%int%" > "%int)
                               outSum inpSum)
                | otherwise =
                    ( False
                    , sformat (int%" inputs could not be resolved")
                              (abs $ resInps - extInps))
        in verifyGeneric [verifier]
    verifyInputs =
        verifyGeneric $ concat $
            zipWith3 inputPredicates [0..] extendedInputs (toList witnesses)

    inputPredicates
        :: Word32                        -- ^ Input index
        -> Maybe (TxIn, VTxLocalContext) -- ^ Input and corresponding output data
        -> TxInWitness
        -> [(Bool, Text)]
    inputPredicates i Nothing _ =
        [(False, sformat ("input #"%int%" is not an unspent output") i)]
    inputPredicates i (Just (txIn@TxIn{..}, lContext)) witness =
        let (txOut@TxOut{..}, _distr) = vtlTxOut lContext in
        [ ( checkAddrHash txOutAddress witness
          , sformat ("input #"%int%"'s witness doesn't match address "%
                     "of corresponding output:\n"%
                     "  input: "%build%"\n"%
                     "  output spent by this input: "%build%"\n"%
                     "  address details: "%addressDetailedF%"\n"%
                     "  witness: "%build)
                i txIn txOut txOutAddress witness
          )
        , case validateTxIn i txIn lContext witness of
              Right _ -> (True, panic "can't happen")
              Left err -> (False, sformat
                  ("input #"%int%" isn't validated by its witness:\n"%
                   "  reason: "%build%"\n"%
                   "  input: "%build%"\n"%
                   "  output spent by this input: "%build%"\n"%
                   "  witness: "%build)
                  i err txIn txOut witness)
        ]

    checkAddrHash addr PkWitness{..}     = checkPubKeyAddress twKey addr
    checkAddrHash addr ScriptWitness{..} = checkScriptAddress twValidator addr

    validateTxIn _i TxIn{..} _ PkWitness{..} =
        if checkSig twKey (txInHash, txInIndex, txOutHash, distrsHash) twSig
            then Right ()
            else Left "signature check failed"
    -- second argument here is local context, can be used for scripts
    validateTxIn _i TxIn{..} _lContext ScriptWitness{..}
        | scrVersion twValidator /= scrVersion twRedeemer =
            Left "validator and redeemer have different versions"
        | not (isKnownScriptVersion (scrVersion twValidator)) =
            Right ()
        | otherwise =
              let txSigData = (txInHash, txInIndex, txOutHash, distrsHash)
              in txScriptCheck txSigData twValidator twRedeemer

verifyTxPure :: Bool
             -> VTxGlobalContext
             -> (TxIn -> Maybe VTxLocalContext)
             -> TxAux
             -> Either Text [TxOutAux]
verifyTxPure verifyAlone gContext resolver =
    runIdentity . verifyTx verifyAlone gContext (Identity . resolver)


----------------------------------------------------------------------------
-- Topsorting
----------------------------------------------------------------------------

data TopsortState a = TopsortState
    { _tsVisited     :: HS.HashSet (Hash Tx)
    , _tsUnprocessed :: [a]
    , _tsResult      :: [a]
    , _tsLoop        :: Bool
    }

$(makeLenses ''TopsortState)

-- | Does topological sort on things that contain transactions – e.g. can be
-- used both for sorting @[Tx]@ and @[(Tx, TxWitness)]@.
--
-- (Backwards dfs from every node with reverse visiting order
-- recording. Returns nothing on loop encountered. Return order is
-- head-first.)
topsortTxs :: forall a. (a -> WithHash Tx) -> [a] -> Maybe [a]
topsortTxs toTx input =
    let res = execState dfs1 initState
    in guard (not $ res ^. tsLoop) >> pure (reverse $ res ^. tsResult)
  where
    dup a = (a,a)
    txHashes :: HashMap (Hash Tx) a
    txHashes = HM.fromList $ map (first (whHash . toTx) . dup) input
    initState = TopsortState HS.empty input [] False
    -- Searches next unprocessed vertix and calls dfs2 for it. Wipes
    -- visited vertices.
    dfs1 :: State (TopsortState a) ()
    dfs1 = unlessM (use tsLoop) $ do
        t <- uses tsUnprocessed head
        whenJust t $ \a -> do
            let tx = toTx a
            ifM (uses tsVisited $ HS.member (whHash tx))
                (tsUnprocessed %= tail)
                (dfs2 HS.empty a tx)
            dfs1
    -- Does dfs putting vertices into tsResult in reversed order of
    -- visiting. visitedThis is map of visited vertices for _this_ dfs
    -- (cycle detection).
    dfs2 :: HashSet (Hash Tx) -> a -> WithHash Tx -> State (TopsortState a) ()
    dfs2 visitedThis _ (WithHash _ txHash)
        | txHash `HS.member` visitedThis = tsLoop .= True
    dfs2 visitedThis a (WithHash tx txHash) = unlessM (use tsLoop) $ do
        tsVisited %= HS.insert txHash
        let visitedNew = HS.insert txHash visitedThis
            dependsUnfiltered =
                mapMaybe (\x -> HM.lookup (txInHash x) txHashes) (txInputs tx)
        depends <- filterM
            (fmap not . uses tsVisited . HS.member . whHash . toTx)
            dependsUnfiltered
        forM_ depends $ \a' -> dfs2 visitedNew a' (toTx a')
        tsResult %= (a:)
