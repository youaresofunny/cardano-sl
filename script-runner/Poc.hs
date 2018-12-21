{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main (main) where

import           AutomatedTestRunner
import           BlockParser ()
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Constraint (Dict (Dict))
import           Data.Default (def)
import           Data.Ix (range)
import qualified Data.Text as T
import           Data.Time.Units (fromMicroseconds)
import           Formatting (Format, int, sformat, (%))
import           NodeControl (NodeInfo (..), genSystemStart, keygen, mkTopo,
                     mutateConfigurationYaml, startNode, stopNode,
                     stopNodeByName)
import           PocMode
import           Pos.Chain.Update (ApplicationName (ApplicationName),
                     BlockVersion (BlockVersion),
                     BlockVersionData (bvdMaxBlockSize, bvdMaxTxSize),
                     BlockVersionModifier (bvmMaxTxSize, bvmMaxBlockSize),
                     SoftwareVersion (SoftwareVersion),
                     ccApplicationVersion_L,
                     ccLastKnownBlockVersion_L)
import qualified Pos.Client.CLI as CLI
import           Pos.Core (Timestamp (..))
import           Pos.DB.Class (gsAdoptedBVData)
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Launcher (Configuration, HasConfigurations, ccUpdate_L,
                     cfoFilePath_L, cfoKey_L, cfoSystemStart_L)
import           Pos.Util.Wlog (logInfo)
import           Prelude (read)
import           Serokell.Data.Memory.Units (Byte)
import           System.Exit (ExitCode (ExitSuccess))
import qualified Turtle as T
import           Types (NodeType (..))
import           Universum hiding (on)

printbvd :: Dict HasConfigurations -> Diffusion PocMode -> PocMode ()
printbvd Dict _diffusion = do
  let
    bvdfmt :: Format r (Byte -> Byte -> r)
    bvdfmt = "BVD: max-tx: " %int% ", max-block: " %int
  bar <- gsAdoptedBVData
  print $ sformat bvdfmt (bvdMaxTxSize bar) (bvdMaxBlockSize bar)

mutateConfiguration :: Configuration -> Configuration
mutateConfiguration cfg = (cfg & ccUpdate_L . ccLastKnownBlockVersion_L .~ BlockVersion 0 1 0) & ccUpdate_L . ccApplicationVersion_L .~ 1

test4 :: Text -> Example ()
test4 stateDir = do
  genesisConfig <- getGenesisConfig
  let
    proposal1 :: Dict HasConfigurations -> Diffusion PocMode -> PocMode ()
    proposal1 Dict diffusion = do
      let
        keyIndex :: Int
        keyIndex = 0
        blockVersion = BlockVersion 0 0 0
        softwareVersion = SoftwareVersion (ApplicationName "cardano-sl") 1
        blockVersionModifier :: BlockVersionModifier
        blockVersionModifier = def -- { bvmMaxTxSize = Just 131072 }
      doUpdate diffusion genesisConfig keyIndex blockVersion softwareVersion blockVersionModifier
    proposal2 :: Dict HasConfigurations -> Diffusion PocMode -> PocMode ()
    proposal2 Dict diffusion = do
      let
        keyIndex :: Int
        keyIndex = 0
        blockVersion = BlockVersion 0 1 0
        softwareVersion = SoftwareVersion (ApplicationName "cardano-sl") 1
        blockVersionModifier :: BlockVersionModifier
        blockVersionModifier = def { bvmMaxBlockSize = Just 1000000 }
      doUpdate diffusion genesisConfig keyIndex blockVersion softwareVersion blockVersionModifier
  onStartup $ \Dict _diffusion -> loadNKeys stateDir 4
  on (1,2) proposal2
  on (1,6) $ \Dict _diffusion -> do
    opts <- view acScriptOptions
    let
      -- the config for the script-runner is mirrored to the nodes it starts
      cfg = opts ^. srCommonNodeArgs . CLI.commonArgs_L
    newConfiguration <- liftIO $ mutateConfigurationYaml (cfg ^. CLI.configurationOptions_L . cfoFilePath_L) (cfg ^. CLI.configurationOptions_L . cfoKey_L) mutateConfiguration
    liftIO $ BS.writeFile (T.unpack $ stateDir <> "/configuration2.yaml") newConfiguration
    let
      cfg2 = cfg & CLI.configurationOptions_L . cfoFilePath_L .~ (T.unpack $ stateDir <> "/configuration2.yaml")
    forM_ (range (0,3)) $ \node -> do
      stopNodeByName (Core, node)
      startNode $ NodeInfo node Core stateDir (stateDir <> "/topology.yaml") cfg2
  on (3,10) $ \Dict _diffusion -> do
    endScript ExitSuccess
  forM_ (range (0,20)) $ \epoch -> on(epoch, 0) printbvd

main :: IO ()
main = do
  systemStart <- genSystemStart 10
  let
    -- cores run from 0-3, relays run from 0-0
    topo = mkTopo 3 0
    systemStartTs :: Timestamp
    systemStartTs = Timestamp $ fromMicroseconds $ (read systemStart) * 1000000
    createNodes :: T.Text -> ScriptRunnerOptions -> PocMode ()
    createNodes stateDir opts = do
      let
        path = stateDir <> "/topology.yaml"
        -- the config for the script-runner is mirrored to the nodes it starts
        cfg = opts ^. srCommonNodeArgs . CLI.commonArgs_L
      liftIO $ do
        BSL.writeFile (T.unpack path) (A.encode topo)
        keygen (cfg ^. CLI.configurationOptions_L)  stateDir
      forM_ (range (0,3)) $ \node -> startNode (NodeInfo node Core stateDir path cfg)
      forM_ (range (0,0)) $ \node -> startNode (NodeInfo node Relay stateDir path cfg)
    cleanupNodes :: PocMode ()
    cleanupNodes = do
      logInfo "stopping all nodes"
      nodeHandles >>= atomically . readTVar >>= liftIO . mapM_ stopNode
    optionsMutator :: ScriptRunnerOptions -> IO ScriptRunnerOptions
    optionsMutator optsin = do
      -- sets the systemStart inside the ScriptRunnerOptions to the systemStart generated at the start of main
      return $ optsin
             & srCommonNodeArgs
             . CLI.commonArgs_L
             . CLI.configurationOptions_L
             . cfoSystemStart_L
             .~ Just systemStartTs
    scriptGetter :: HasEpochSlots => Text -> PocMode Script
    scriptGetter stateDir = return $ getScript $ test4 stateDir
    runScript' :: Text -> IO ()
    runScript' stateDir = do
      runScript (createNodes stateDir) cleanupNodes optionsMutator (scriptGetter stateDir)
  T.with (T.mktempdir "/tmp" "script-runner") $ \stateDir -> do
    runScript' $ T.pack $ T.encodeString stateDir
