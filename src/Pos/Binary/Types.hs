{-# LANGUAGE UndecidableInstances #-}

-- | Binary serialization of Pos.Types.* modules

module Pos.Binary.Types () where

import           Data.Binary.Get     (getInt32be, getWord64be, getWord8, label)
import           Data.Binary.Put     (putInt32be, putWord64be, putWord8)
import           Formatting          (int, sformat, (%))
import           Universum

import           Pos.Binary.Class    (Bi (..), UnsignedVarInt (..))
import           Pos.Binary.Merkle   ()
import           Pos.Binary.Update   ()
import           Pos.Binary.Version  ()
import           Pos.Constants       (protocolMagic)
import qualified Pos.Data.Attributes as A
import           Pos.Ssc.Class.Types (Ssc (..))
import qualified Pos.Types.Timestamp as T
import qualified Pos.Types.Types     as T

-- kind of boilerplate, but anyway that's what it was made for --
-- verbosity and clarity

instance Bi (A.Attributes ()) where
    get = label "Attributes" $
        A.getAttributes (\_ () -> Nothing) (128 * 1024 * 1024) ()
    put = A.putAttributes (\() -> [])

instance Bi T.Coin where
    put = putWord64be . T.unsafeGetCoin
    get = T.mkCoin <$> getWord64be

instance Bi T.Timestamp where
    get = fromInteger <$> get
    put = put . toInteger

instance Bi T.EpochIndex where
    get = T.EpochIndex . getUnsignedVarInt <$> get
    put (T.EpochIndex c) = put (UnsignedVarInt c)

instance Bi T.LocalSlotIndex where
    get = T.LocalSlotIndex . getUnsignedVarInt <$> get
    put (T.LocalSlotIndex c) = put (UnsignedVarInt c)

instance Bi T.SlotId where
    put (T.SlotId e s) = put e >> put s
    get = T.SlotId <$> get <*> get

instance Bi T.TxIn where
    put (T.TxIn hash index) = put hash >> put (UnsignedVarInt index)
    get = label "TxIn" $ T.TxIn <$> get <*> (getUnsignedVarInt <$> get)

instance Bi T.TxOut where
    put (T.TxOut addr coin) = put addr >> put coin
    get = label "TxOut" $ T.TxOut <$> get <*> get

instance Bi T.Tx where
    put (T.Tx ins outs attrs) = put ins >> put outs >> put attrs
    get = label "Tx" $ T.Tx <$> get <*> get <*> get

instance Bi T.TxInWitness where
    put (T.PkWitness key sig)     = putWord8 0 >> put key >> put sig
    put (T.ScriptWitness val red) = putWord8 1 >> put val >> put red
    get = label "TxInWitness" $ do
        tag <- getWord8
        case tag of
            0 -> T.PkWitness <$> get <*> get
            1 -> T.ScriptWitness <$> get <*> get
            t -> fail $ "get@TxInWitness: unknown tag " <> show t

instance Bi T.TxDistribution where
    put (T.TxDistribution ds) =
        put $ if all null ds
                  then Left (UnsignedVarInt (length ds))
                  else Right ds
    get = label "TxDistribution" $
        T.TxDistribution .
        either (\(UnsignedVarInt n) -> replicate n []) identity
            <$> get

instance Bi T.Undo where
    put (T.Undo txs psks) = put txs >> put psks
    get = label "Undo" $ T.Undo <$> get <*> get

-- serialized as vector of TxInWitness
--instance Bi T.TxWitness where

instance Bi T.SharedSeed where
    put (T.SharedSeed bs) = put bs
    get = T.SharedSeed <$> get

----------------------------------------------------------------------------
-- Generic block header
----------------------------------------------------------------------------

instance ( Bi (T.BodyProof b)
         , Bi (T.ConsensusData b)
         , Bi (T.ExtraHeaderData b)
         ) =>
         Bi (T.GenericBlockHeader b) where
    put T.GenericBlockHeader{..} = do
        putInt32be protocolMagic
        put _gbhPrevBlock
        put _gbhBodyProof
        put _gbhConsensus
        put _gbhExtra
    get =
        label "GenericBlockHeader" $ do
        blockMagic <- getInt32be
        when (blockMagic /= protocolMagic) $
            fail $ "GenericBlockHeader failed with wrong magic: " <> show blockMagic
        T.GenericBlockHeader <$> get <*> get <*> get <*> get

instance ( Bi (T.BodyProof b)
         , Bi (T.ConsensusData b)
         , Bi (T.ExtraHeaderData b)
         , Bi (T.Body b)
         , Bi (T.ExtraBodyData b)
         ) =>
         Bi (T.GenericBlock b) where
    put T.GenericBlock{..} = do
        put _gbHeader
        put _gbBody
        put _gbExtra
    get = label "GenericBlock" $ T.GenericBlock <$> get <*> get <*> get

----------------------------------------------------------------------------
-- MainBlock
----------------------------------------------------------------------------

instance Bi T.ChainDifficulty where
    get = T.ChainDifficulty . getUnsignedVarInt <$> get
    put (T.ChainDifficulty c) = put (UnsignedVarInt c)

instance Ssc ssc => Bi (T.BodyProof (T.MainBlockchain ssc)) where
    put T.MainProof{..} = do
        put (UnsignedVarInt mpNumber)
        put mpRoot
        put mpWitnessesHash
        put mpMpcProof
    get = label "MainProof" $
        T.MainProof
            <$> (getUnsignedVarInt <$> get)
            <*> get
            <*> get
            <*> get

instance Bi (T.BlockSignature ssc) where
    put (T.BlockSignature sig)             = putWord8 0 >> put sig
    put (T.BlockPSignatureEpoch proxySig)  = putWord8 1 >> put proxySig
    put (T.BlockPSignatureSimple proxySig) = putWord8 2 >> put proxySig
    get = label "BlockSignature" $ getWord8 >>= \case
        0 -> T.BlockSignature <$> get
        1 -> T.BlockPSignatureEpoch <$> get
        2 -> T.BlockPSignatureSimple <$> get
        t -> fail $ "get@BlockSignature: unknown tag: " <> show t

instance Bi (T.ConsensusData (T.MainBlockchain ssc)) where
    put T.MainConsensusData{..} = do
        put _mcdSlot
        put _mcdLeaderKey
        put _mcdDifficulty
        put _mcdSignature
    get = label "MainConsensusData" $ T.MainConsensusData <$> get <*> get <*> get <*> get

instance Ssc ssc => Bi (T.Body (T.MainBlockchain ssc)) where
    put T.MainBody{..} = do
        put _mbTxs
        put _mbWitnesses
        put _mbTxAddrDistributions
        put _mbMpc
        put _mbProxySKs
    get = label "MainBody" $ do
        _mbTxs <- get
        _mbWitnesses <- get
        _mbTxAddrDistributions <- get
        _mbMpc <- get
        _mbProxySKs <- get
        let lenTxs    = length _mbTxs
            lenWit    = length _mbWitnesses
            lenDistrs = length _mbTxAddrDistributions
        when (lenTxs /= lenWit) $ fail $ toString $
            sformat ("get@(Body MainBlockchain): "%
                     "size of txs tree ("%int%") /= "%
                     "length of witness list ("%int%")")
                    lenTxs lenWit
        when (lenTxs /= lenDistrs) $ fail $ toString $
            sformat ("get@(Body MainBlockchain): "%
                     "size of txs tree ("%int%") /= "%
                     "length of address distrs list ("%int%")")
                    lenTxs lenDistrs
        for_ (zip3 [0 :: Int ..] (toList _mbTxs) _mbTxAddrDistributions) $
            \(i, tx, ds) -> do
                let lenOut = length (T.txOutputs tx)
                    lenDist = length (T.getTxDistribution ds)
                when (lenOut /= lenDist) $ fail $ toString $
                    sformat ("get@(Body MainBlockchain): "%
                             "amount of outputs ("%int%") of tx "%
                             "#"%int%" /= amount of distributions "%
                             "for this tx ("%int%")")
                            lenOut i lenDist
        return T.MainBody{..}

----------------------------------------------------------------------------
-- GenesisBlock
----------------------------------------------------------------------------

instance Bi (T.BodyProof (T.GenesisBlockchain ssc)) where
    put (T.GenesisProof h) = put h
    get = label "GenesisProof" $ T.GenesisProof <$> get

instance Bi (T.ConsensusData (T.GenesisBlockchain ssc)) where
    put T.GenesisConsensusData{..} = put _gcdEpoch >> put _gcdDifficulty
    get = label "GenesisConsensusData" $ T.GenesisConsensusData <$> get <*> get

instance Bi (T.Body (T.GenesisBlockchain ssc)) where
    put (T.GenesisBody leaders) = put leaders
    get = label "GenesisBody" $ T.GenesisBody <$> get

instance Bi T.MainExtraHeaderData where
    put T.MainExtraHeaderData {..} =  put _mehProtocolVersion
                                   *> put _mehSoftwareVersion
                                   *> put _mehAttributes
    get = label "MainExtraHeaderData" $ T.MainExtraHeaderData <$> get <*> get <*> get

instance Bi T.MainExtraBodyData where
   put T.MainExtraBodyData {..} =  put _mebAttributes
                                *> put _mebUpdate
                                *> put _mebUpdateVotes
   get = label "MainExtraBodyData" $ T.MainExtraBodyData <$> get <*> get <*> get
