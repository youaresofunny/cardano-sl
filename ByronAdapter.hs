{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar)
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy (Proxy (..))
import Data.Tagged (Tagged (..), tagWith)

import Pos.Chain.Block (Block, BlockHeader, HasHeaderHash, HeaderHash, getBlockHeader, headerHash)
import Pos.Chain.Security (SecurityParams)
import Pos.Chain.Ssc (MCCommitment (..), MCOpening (..), MCShares (..),
                      MCVssCertificate (..), getCertId)
import Pos.Chain.Txp (TxAux (..), TxMsgContents (..))
import Pos.Chain.Update (BlockVersionData, UpdateProposal (..), UpdateVote (..))
import Pos.Communication (NodeId)
import Pos.Core (StakeholderId, addressHash)
import Pos.Core.Chrono (NewestFirst (..))
import Pos.Crypto (hash, toPublic)
import Pos.DB.Block (GetHeadersFromManyToError (..), GetHashesRangeError (..))
import Pos.Diffusion.Full (FullDiffusionConfiguration (..), diffusionLayerFull)
import Pos.Infra.Diffusion.Types
-- An ancient relic. Needed for the network configuration type.
import Pos.Infra.DHT.Real.Param (KademliaParams)
import Pos.Infra.Network.Types (NetworkConfig (..))
import Pos.Logic.Types hiding (streamBlocks)
import qualified Pos.Logic.Types as Logic
import Pos.Util.Trace (noTrace, wlogTrace)
import Pos.Util.Wlog.Compatibility (setupTestLogging)
import Pos.Launcher.Resource (loggerBracket)

-- Stuff for bringing up a node (arguments etc.)
import Pos.Behavior (bcSecurityParams)
import Pos.Chain.Block (recoveryHeadersMessage, streamWindow)
import Pos.Chain.Update (lastKnownBlockVersion)
import Pos.Configuration (networkConnectionTimeout)
import Pos.Client.CLI (SimpleNodeArgs (..), commonArgs, configurationOptions,
                       getSimpleNodeOptions, loggingParams)
import Pos.Client.CLI.Params (getNodeParams)
import Pos.Chain.Genesis (Config (configGeneratedSecrets), GenesisHash (..),
                          configGenesisHash, configProtocolConstants,
                          configProtocolMagic, configBlockVersionData)
import Pos.Launcher (LoggingParams (lpDefaultName), NodeParams (..), cfoKey, withConfigurations)
import Pos.Util.CompileInfo (withCompileInfo)

-- | Run a 'ByronAdapter' against a diffusion layer using 'STM' on a map of
-- peers to their announced headers to drive the adapter choices.
byronAdapterIO
  :: TVar (Map NodeId BlockHeader)
  -> Diffusion IO
  -> ByronAdapter NodeId HeaderHash BlockHeader Block IO t
  -> IO t
byronAdapterIO peerHeadersVar diffusion ba =
  atomically (readTVar peerHeadersVar) >>= goNext ba peerHeadersVar
  where

  goNext ba peerHeadersVar peerHeaders = do
    next <- runByronAdapter ba peerHeaders
    case next of
      Done t -> pure t
      Wait ba' -> waitForChange ba' peerHeadersVar peerHeaders
      Take peer checkpoints ba' download -> case Map.lookup peer peerHeaders of
        Nothing -> waitForChange ba' peerHeadersVar peerHeaders
        Just header -> do
          let hh = headerHash header
          it <- streamBlocks diffusion peer hh checkpoints (goDownload download)
          case it of
            Nothing -> error "server does not support streaming"
            Just ba' -> goNext ba' peerHeadersVar peerHeaders

  goDownload baDownload = StreamBlocks
    { streamBlocksDone = downloadFinished baDownload
    , streamBlocksMore = \blks -> do
        baDownload' <- downloadBatch baDownload blks
        pure (goDownload baDownload')
    }

  waitForChange ba peerHeadersVar peerHeaders = do
    peerHeaders' <- atomically $ do
      peerHeaders' <- readTVar peerHeadersVar
      when (peerHeaders == peerHeaders') retry
      pure peerHeaders'
    goNext ba peerHeadersVar peerHeaders'

data ByronAdapter peer headerHash header block m t = ByronAdapter
  { runByronAdapter :: Map peer header -> m (Next peer headerHash header block m t)
  }

data Next peer headerHash header block m t where
  Done :: t -> Next peer headerHash header block m t
  -- | No action now. Run this when there's a change.
  Wait
    :: ByronAdapter peer headerHash header block m t
    -> Next peer headerHash header block m t
  -- | Get a particular chain.
  Take
    :: peer         -- ^ Try to get the chain from this peer ...
    -> [headerHash] -- ^ ... using these checkpoints.
                    --   The tip header is determined from the peer's
                    --   announcement.
    -> ByronAdapter peer headerHash header block m t
       -- ^ This is used in case the peer cannot deliver the chain (for instance
       --   because it changed its tip in the interim time.
    -> Download peer headerHash header block m t
       -- ^ This is how to download it.
    -> Next peer headerHash header block m t

data Download peer headerHash header block m t = Download
  { downloadBatch    :: NonEmpty block -> m (Download peer headerHash header block m t)
  , downloadFinished :: m (ByronAdapter peer headerHash header block m t)
  }

data ByronAdapterRequirements m = ByronAdapterRequirements
  { barStakeholderId   :: StakeholderId
  , barSecurityParams  :: SecurityParams
  , barAdoptedBVData   :: m BlockVersionData
    -- | Needed to supply an answer to 'getTip' and 'getTipHeader' for the
    -- logic layer. It'll look to any peers that we're serving up the genesis
    -- block only.
  , barGenesisBlock    :: Block
  , barNetworkConfig   :: NetworkConfig KademliaParams
  , barDiffusionConfig :: FullDiffusionConfiguration
  }

-- | Make a 'Logic' for the Byron adapter. It ignores all data but for blocks
-- and block headers. Posting a block header puts it into the 'TVar' map.
byronAdapterLogic
  :: ByronAdapterRequirements IO
  -> TVar (Map NodeId BlockHeader)
  -> Logic IO
byronAdapterLogic bar tipsTVar =
  let -- When a block header comes in, just put it in the 'TVar'.
      postBlockHeader :: BlockHeader -> NodeId -> IO ()
      postBlockHeader bh peer = atomically $ modifyTVar' tipsTVar (Map.insert peer bh)
      -- Helpful for creating the 'KeyVal' for ssc-related stuff.
      sscKeyValCommon
        :: forall contents m .
           ( Applicative m )
        => (contents -> StakeholderId)
        -> contents
        -> m (Tagged contents StakeholderId)
      sscKeyValCommon contentsToKey = \contents ->
        pure (tagWith (Proxy :: Proxy contents) (contentsToKey contents))
      logic = Logic
        { ourStakeholderId   = barStakeholderId bar
        , getAdoptedBVData   = barAdoptedBVData bar
        -- We have no blocks to serve, so the next 5 are trivial.
        , getSerializedBlock = const (pure Nothing)
        , Logic.streamBlocks   = const (pure ())
        , Logic.getBlockHeader = const (pure Nothing)
        , getHashesRange     = \_ _ _ -> pure (Left (GHRBadInput "this type is not expressive enough"))
        , getBlockHeaders    = \_ _ _ -> pure (Left (GHFBadInput "this type is not expressive enough"))
        -- This one is only needed if
        -- 1. serving a stream of blocks
        -- 2. requesting in batch mode
        -- So we leave it out: it says that nothing is in the main chain.
        , getLcaMainChain    = \lst -> pure (NewestFirst [], lst)
        , getTip             = pure (barGenesisBlock bar)
        , getTipHeader       = pure (Pos.Chain.Block.getBlockHeader (barGenesisBlock bar))
        -- Block headers just go into the map, overwriting the old one.
        -- Diffusion layer will be responsible for ensuring the map shrinks
        -- when a peer goes away.
        , postBlockHeader    = postBlockHeader
        -- Non-block data: ignore it all. We need the 'toKey' functions though.
        , postTx             = ignoreKeyVal (pure . Tagged . hash . taTx . getTxMsgContents)
        , postUpdate         = ignoreKeyVal (pure . tagWith (Proxy :: Proxy (UpdateProposal, [UpdateVote])) . hash . fst)
        , postVote           = ignoreKeyVal (\uv -> pure (tagWith (Proxy :: Proxy UpdateVote) (uvProposalId uv, uvKey uv, uvDecision uv)))
        -- TODO these are so cumbersome to get right.
        , postSscCommitment  = ignoreKeyVal (sscKeyValCommon (\(MCCommitment (pk, _, _)) -> addressHash pk))
        , postSscOpening     = ignoreKeyVal (sscKeyValCommon (\(MCOpening key _) -> key))
        , postSscShares      = ignoreKeyVal (sscKeyValCommon (\(MCShares key _) -> key))
        , postSscVssCert     = ignoreKeyVal (sscKeyValCommon (\(MCVssCertificate vc) -> getCertId vc))
        , postPskHeavy       = const (pure False)
        -- No recovery mode. It's useless.
        , recoveryInProgress = pure False
        , securityParams     = barSecurityParams bar
        }

  in  logic


-- | Run a 'ByronAdapter' supported by a full diffusion layer.
runByronAdapterIO
  :: ByronAdapterRequirements IO
  -> ByronAdapter NodeId HeaderHash BlockHeader Block IO t
  -> IO t
runByronAdapterIO bar ba = do
  tipsTVar :: TVar (Map NodeId BlockHeader) <- newTVarIO mempty
  let mkLogic = const (byronAdapterLogic bar tipsTVar)
  diffusionLayerFull fdconf networkConfig Nothing mkLogic $ \diffusionLayer -> do
    runDiffusionLayer diffusionLayer $ do
      putStrLn "Welcome to the Byron block download adapter demo!"
      putStrLn "'next' to wait for a change in peers/headers"
      putStrLn "'take' to download the chain from a given peer"
      putStrLn "'done' to quit (not working yet, use ctrl+c)"
      t <- byronAdapterIO tipsTVar (diffusion diffusionLayer) ba
      -- FIXME: diffusion layer doesn't shut down because there is an active
      -- subscription.
      putStrLn "Bye. cardano-sl can only be shut down by killing it."
      pure t
  where
  networkConfig = barNetworkConfig bar
  fdconf = barDiffusionConfig bar

-- | Simple stdio adapter which allows the user to pick a download target and
-- see an indication of the block download.
interactiveByronAdapter
  :: ( Show peer
     , Show block
     , HasHeaderHash header
     , HasHeaderHash block
     )
  => GenesisBlock
  -> ByronAdapter peer HeaderHash header block IO ()
interactiveByronAdapter genesisBlock = ByronAdapter $ \map -> do
  let mapWithHashes = fmap headerHash map
  putStrLn $ "Available peers: " ++ show mapWithHashes
  inputLoop map

  where

  inputLoop map = do
    inp <- getLine
    case inp of
      "done" -> pure (Done ())
      "next" -> do
        putStrLn "Ok, waiting for a change..."
        pure (Wait (interactiveByronAdapter genesisBlock))
      "take" -> inputLoopTake map

  inputLoopTake map = do
    putStrLn "From which peer? (give a 0-based index)"
    let indexLoop = do
          nStr <- getLine
          case reads nStr of
            [(n :: Word, "")] -> pure (fromIntegral n)
            _ -> indexLoop
    n <- indexLoop
    if n >= length map
    then do
      putStrLn "Index is too big."
      inputLoopTake map
    else do
      let (peer, header) = Map.toList map !! n
          checkpoints = [headerHash genesisBlock]
      pure (Take peer checkpoints (interactiveByronAdapter genesisBlock) download)

  download = Download
    { downloadBatch = \batch -> do
        print (fmap headerHash batch)
        pure download
    , downloadFinished = pure (interactiveByronAdapter genesisBlock)
    }

-- withCompileInfo forced upon us by 'getSimpleNodeOptions'. It uses it to
-- print the git revision...
main :: IO ()
main = withCompileInfo $ do
  SimpleNodeArgs cArgs nArgs <- getSimpleNodeOptions
  let lArgs = loggingParams "node" cArgs
      confOpts = configurationOptions (commonArgs cArgs)
      confKey = cfoKey confOpts
  -- We must 'setupTestLogging' from Pos.Util.Wlog.Compatibility or else
  -- the program will crash... there is no emoji for this.
  --setupTestLogging 
  --setupLogging
  loggerBracket confKey lArgs $
    withConfigurations Nothing Nothing False confOpts $ \genesisConfig _ _ _ -> do
      (nodeParams, Just sscParams) <- getNodeParams
        (lpDefaultName lArgs)
        cArgs
        nArgs
        (configGeneratedSecrets genesisConfig)
      let genesisBlock = genesisBlock0 (configProtocolMagic genesisConfig)
                                       (configGenesisHash genesisConfig)
                                       (genesisLeaders genesisConfig)
          genesisBlockHash = headerHash genesisHash
          bar = ByronAdapterRequirements
            { barStakeholderId = addressHash (toPublic (npSecretKey nodeParams))
            , barSecurityParams = bcSecurityParams (npBehaviorConfig nodeParams)
              -- No database, no BV data change.
            , barAdoptedBVData = pure (configBlockVersionData genesisConfig)
            , barGenesisBlock = genesisBlock
            , barNetworkConfig = npNetworkConfig nodeParams
            , barDiffusionConfig = FullDiffusionConfiguration
                { fdcProtocolMagic = configProtocolMagic genesisConfig
                , fdcProtocolConstants = configProtocolConstants genesisConfig
                , fdcRecoveryHeadersMessage = recoveryHeadersMessage
                , fdcLastKnownBlockVersion = lastKnownBlockVersion
                , fdcConvEstablishTimeout = networkConnectionTimeout
                -- diffusion layer is responsible for all logging, since this
                -- executable doesn't do any logic-layer stuff.
                , fdcTrace = noTrace -- wlogTrace "diffusion"
                , fdcStreamWindow = streamWindow
                }
            }
      runByronAdapterIO bar (interactiveByronAdapter (configGenesisHash genesisConfig))
      pure ()
