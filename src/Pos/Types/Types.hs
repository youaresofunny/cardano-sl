{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Definitions of the most fundamental types.

module Pos.Types.Types
       (
         NodeId (..)
       , nodeF

       , EpochIndex
       , LocalSlotIndex
       , SlotId (..)
       , FlatSlotId

       , Coin (..)
       , coinF

       , Address (..)
       , addressF

       , TxId
       , TxIn (..)
       , TxOut (..)
       , Tx (..)

       , Utxo
       , AddrId

       , RandomSecret
       , Commitment (..)
       , Opening (..)

       , ChainDifficulty (..)
       , Payload (..)
       , CommitmentsMap
       , OpeningsMap
       , SharesMap
       , HeaderHash
       , BlockHeader (..)
       , SignedBlockHeader (..)
       , GenericBlock (..)
       , TxsPayload
       , TxsProof (..)
       , Block
       , GenesisBlock (..)

       , Entry (..)
       , Blockkk
       , displayEntry
       ) where

import           Data.Binary         (Binary)
import           Data.Binary.Orphans ()
import           Data.SafeCopy       (SafeCopy (..), base, contain, deriveSafeCopySimple,
                                      safeGet, safePut)
import qualified Data.Text           as T (unwords)
import           Data.Text.Buildable (Buildable)
import qualified Data.Text.Buildable as Buildable
import           Data.Vector         (Vector)
import           Data.Word           (Word32, Word64)
import           Formatting          (Format, bprint, build, int, sformat, shown, (%))
import           Universum

import           Pos.Crypto          (EncShare, Hash, PublicKey, Secret, SecretProof,
                                      Share, Signature)
import           Pos.Merkle          (MerkleRoot)
import           Pos.Util            (Raw)

----------------------------------------------------------------------------
-- Node. TODO: do we need it?
----------------------------------------------------------------------------

newtype NodeId = NodeId
    { getNodeId :: Int
    } deriving (Show, Eq, Ord, Enum, Binary)

instance Buildable NodeId where
    build = bprint ("#"%int) . getNodeId

nodeF :: Format r (NodeId -> r)
nodeF = build

----------------------------------------------------------------------------
-- Slotting
----------------------------------------------------------------------------

-- | Index of epoch.
type EpochIndex = Word64

-- | Index of slot inside a concrete epoch.
type LocalSlotIndex = Word16

-- | Slot is identified by index of epoch and local index of slot in
-- this epoch. This is a global index
data SlotId = SlotId
    { siEpoch :: !EpochIndex
    , siSlot  :: !LocalSlotIndex
    } deriving (Show, Eq, Generic)

instance Binary SlotId

-- | FlatSlotId is a flat version of SlotId
type FlatSlotId = Word64

----------------------------------------------------------------------------
-- Coin
----------------------------------------------------------------------------

-- | Coin is the least possible unit of currency.
newtype Coin = Coin
    { getCoin :: Int64
    } deriving (Num, Enum, Integral, Show, Ord, Real, Generic, Eq, Binary)

instance Buildable Coin where
    build = bprint (int%" coin(s)")

-- | Coin formatter which restricts type.
coinF :: Format r (Coin -> r)
coinF = build

----------------------------------------------------------------------------
-- Address
----------------------------------------------------------------------------

-- | Address is where you can send coins.
newtype Address = Address
    { getAddress :: PublicKey
    } deriving (Show, Eq, Generic, Buildable, Ord, Binary)

addressF :: Format r (Address -> r)
addressF = build

----------------------------------------------------------------------------
-- Transaction
----------------------------------------------------------------------------

type TxId = Hash Tx

-- | Transaction input.
data TxIn = TxIn
    { txInHash  :: !TxId  -- ^ Which transaction's output is used
    , txInIndex :: !Word32     -- ^ Index of the output in transaction's
                               -- outputs
    } deriving (Eq, Ord, Show, Generic)

instance Binary TxIn

instance Buildable TxIn where
    build TxIn {..} = bprint ("TxIn ("%build%", "%int%")") txInHash txInIndex

-- | Transaction output.
data TxOut = TxOut
    { txOutAddress :: !Address
    , txOutValue   :: !Coin
    } deriving (Eq, Ord, Show, Generic)

instance Binary TxOut

instance Buildable TxOut where
    build TxOut {..} =
        bprint ("TxOut ("%build%", "%coinF%")") txOutAddress txOutValue

-- | Transaction.
data Tx = Tx
    { txInputs  :: ![TxIn]   -- ^ Inputs of transaction.
    , txOutputs :: ![TxOut]  -- ^ Outputs of transaction.
    } deriving (Eq, Ord, Show, Generic)

instance Binary Tx

----------------------------------------------------------------------------
-- UTXO
----------------------------------------------------------------------------

-- | Unspent transaction outputs.
type Utxo = HashMap AddrId Address

-- | 'AddrId' identifies usage of address as output of transaction.
-- Basically, it is tuple of transaction identifier, index in list of outputs
-- and associated value.
type AddrId = (TxId, Int, Coin)

----------------------------------------------------------------------------
-- MPC
----------------------------------------------------------------------------

-- | Random secret is generated by each party at the beginning of MPC. It is
-- also result of MPC which is used as seed for follow-the-satoshi.
type RandomSecret = Secret

-- | Commitment is a message generated during the first stage of
-- MPC. It contains encrypted shares and proof of secret.
data Commitment = Commitment
    { commProof  :: !SecretProof
    , commShares :: !(HashMap PublicKey (EncShare))
    } deriving (Show, Eq, Generic)

instance Binary Commitment

-- | Opening reveals message.
-- Maybe we'll need to add something here.
newtype Opening = Opening
    { getOpening :: RandomSecret
    } deriving (Show, Eq, Generic, Binary, Buildable)

----------------------------------------------------------------------------
-- Block
----------------------------------------------------------------------------

newtype ChainDifficulty = ChainDifficulty
    { getChainDifficulty :: Word64
    } deriving (Show, Eq, Ord, Num, Binary)

class Payload p where
    type Proof p :: *

    checkProof :: p -> Proof p -> Bool

type CommitmentsMap = HashMap PublicKey Commitment
type OpeningsMap = HashMap PublicKey Opening

-- | For each node which generated a 'RandomSecret', the shares map gives us
-- keys and corresponding shares sent to those keys. Specifically, if node X
-- has generated a secret and sent a share to node Y, here's how to get this
-- share: @sharesMap ! X ! Y@.
type SharesMap = HashMap PublicKey (HashMap PublicKey Share)

type HeaderHash proof = Hash (SignedBlockHeader proof)

-- | Header of block contains all the information necessary to
-- validate consensus algorithm. It also contains proof of payload
-- associated with it.
data BlockHeader proof = BlockHeader
    { -- | Hash of the previous block's header.
      bhPrevHash     :: !(HeaderHash proof)
    , -- | Id of the slot for which this block was generated.
      bhSlot         :: !SlotId
    , -- | Public key of slot leader. Maybe later we'll see it is redundant.
      bhLeaderKey    :: !PublicKey
    , -- | Commitments are added during the first phase of epoch.
      bhCommitments  :: !CommitmentsMap
    , -- | Openings are added during the second phase of epoch.
      bhOpenings     :: !OpeningsMap
    , -- | Decrypted shares to be used in the second phase.
      bhShares       :: !SharesMap
    , -- | Difficulty of chain ending in this block.
      bhDifficulty   :: !ChainDifficulty
    , -- | Proof of payload.
      bhPayloadProof :: !proof
    } deriving (Show, Eq, Generic)

instance Binary p => Binary (BlockHeader p)

-- | SignedBlockHeader consists of BlockHeader and its signature.
data SignedBlockHeader proof = SignedBlockHeader
    { sbhHeader    :: !(BlockHeader proof)
    , sbhSignature :: !(Signature (BlockHeader proof))
    } deriving (Show, Eq, Generic)

instance Binary p => Binary (SignedBlockHeader p)

-- | In general Block consists of some payload and header associated
-- with it.
data GenericBlock payload = GenericBlock
    { gbHeader  :: !(SignedBlockHeader (Proof payload))
    , gbPayload :: !payload
    }

-- | In our cryptocurrency, payload is a list of transactions.
type TxsPayload = [Tx]

-- | Proof of transactions list.
data TxsProof = TxsProof
    { tpNumber :: !Word32
    , tpRoot   :: !(MerkleRoot Tx)
    } deriving (Show, Eq, Generic)

instance Payload TxsPayload where
    type Proof TxsPayload = TxsProof
    checkProof _ _ = True

instance Binary TxsProof

type Block = GenericBlock TxsPayload

-- | Genesis block doesn't have any payload and is not strictly
-- necessary. However, it is good idea to store list of leaders
-- somewhere instead of calculating it every time. If we decide to
-- make it part of blockchain, we will need to store hash also.
data GenesisBlock = GenesisBlock
    { gbLeaders :: !(Vector PublicKey)
    } deriving (Generic)

----------------------------------------------------------------------------
-- Block. Leftover.
----------------------------------------------------------------------------

-- | An entry in a block
data Entry

      -- | Transaction
    = ETx Tx

      -- | Hash of random string U that a node has committed to
    | EUHash NodeId (Hash Raw)
      -- | An encrypted piece of secret-shared U that the first node sent to
      -- the second node (and encrypted with the second node's pubkey)
    | EUShare NodeId NodeId EncShare
      -- | Leaders for a specific epoch
    | ELeaders Int [NodeId]

    deriving (Eq, Ord, Show)

-- | Block
type Blockkk = [Entry]

displayEntry :: Entry -> Text
displayEntry (ETx tx) =
    "transaction " <> show tx
displayEntry (EUHash nid h) =
    sformat (nodeF%"'s commitment = "%shown) nid h
displayEntry (EUShare n_from n_to share) =
    sformat (nodeF%"'s share for "%nodeF%" = "%build) n_from n_to share
displayEntry (ELeaders epoch leaders) =
    sformat ("leaders for epoch "%int%" = "%build)
            epoch
            (T.unwords (map (toS . sformat nodeF) leaders))

----------------------------------------------------------------------------
-- SafeCopy instances
----------------------------------------------------------------------------

-- These instances are all gathered at the end because otherwise we'd have to
-- sort types topologically

deriveSafeCopySimple 0 'base ''NodeId
deriveSafeCopySimple 0 'base ''SlotId
deriveSafeCopySimple 0 'base ''Coin
deriveSafeCopySimple 0 'base ''Address
deriveSafeCopySimple 0 'base ''TxIn
deriveSafeCopySimple 0 'base ''TxOut
deriveSafeCopySimple 0 'base ''Tx
deriveSafeCopySimple 0 'base ''Commitment
deriveSafeCopySimple 0 'base ''Opening
deriveSafeCopySimple 0 'base ''ChainDifficulty
deriveSafeCopySimple 0 'base ''BlockHeader
deriveSafeCopySimple 0 'base ''SignedBlockHeader

-- This instance can't be derived because 'deriveSafeCopySimple' is not
-- clever enough to add a “SafeCopy (Proof payload) =>” constaint.
instance (SafeCopy (Proof payload), SafeCopy payload) =>
         SafeCopy (GenericBlock payload) where
    getCopy = contain $ do
        gbHeader <- safeGet
        gbPayload <- safeGet
        return GenericBlock{..}
    putCopy GenericBlock{..} = contain $ do
        safePut gbHeader
        safePut gbPayload

deriveSafeCopySimple 0 'base ''TxsProof
deriveSafeCopySimple 0 'base ''GenesisBlock
deriveSafeCopySimple 0 'base ''Entry
