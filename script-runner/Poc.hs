{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main (main) where

import qualified Data.ByteString as BS
import           Data.Constraint (Dict (Dict))
import           Data.Default (def)
import           Data.Ix (range)
import qualified Data.Text as T
import           System.Exit (ExitCode (ExitSuccess))
import           Universum hiding (on)

import           Pos.Chain.Update (ApplicationName (ApplicationName),
                     BlockVersion (BlockVersion),
                     BlockVersionModifier (bvmMaxBlockSize),
                     SoftwareVersion (SoftwareVersion), ccApplicationVersion_L,
                     ccLastKnownBlockVersion_L)
import qualified Pos.Client.CLI as CLI
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Launcher (Configuration, HasConfigurations, ccUpdate_L,
                     cfoFilePath_L, cfoKey_L)

import           AutomatedTestRunner
import           BlockParser ()
import           NodeControl (NodeInfo (..), mutateConfigurationYaml, startNode,
                     stopNodeByName)
import           PocMode
import           Types (NodeType (..), Todo (Todo))

mutateConfiguration :: Configuration -> Configuration
mutateConfiguration cfg = (cfg & ccUpdate_L . ccLastKnownBlockVersion_L .~ BlockVersion 0 1 0) & ccUpdate_L . ccApplicationVersion_L .~ 1

test4 :: Example ()
test4 = do
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
  onStartup $ \Dict _diffusion -> do
    stateDir <- view acStatePath
    loadNKeys stateDir 4
  on (1,2) proposal2
  on (1,6) $ \Dict _diffusion -> do
    stateDir <- view acStatePath
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
  forM_ (range (0,20)) $ \epoch -> do
    on(epoch, 0) $ printbvd epoch 0
    on(epoch, 1) $ printbvd epoch 1

main :: IO ()
main = do
  runScript $ ScriptParams
    { spTodo = (Todo 4)
    , spScript = test4
    , spRecentSystemStart = True
    , spStartCoreAndRelay = True
    }
