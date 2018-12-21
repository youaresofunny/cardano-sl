{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types (NodeHandle(..), NodeType(..), ScriptRunnerOptions(..), ScriptRunnerUIMode(..), srCommonNodeArgs, srPeers, srUiMode) where

import           Control.Concurrent.Async.Lifted.Safe
import           Control.Lens (makeLenses, to)
import           System.Process
import           Universum

import qualified Pos.Client.CLI as CLI
import           Pos.Infra.Network.Types (NetworkConfig (ncDequeuePolicy, ncEnqueuePolicy, ncFailurePolicy, ncTopology),
                     NodeId, Topology (TopologyAuxx), topologyDequeuePolicy,
                     topologyEnqueuePolicy, topologyFailurePolicy)

data NodeHandle = NodeHandle (Async ()) ProcessHandle
data NodeType = Core | Relay deriving (Eq, Ord, Show)
data ScriptRunnerUIMode = BrickUI | PrintUI deriving Show

data ScriptRunnerOptions = ScriptRunnerOptions
  { _srCommonNodeArgs :: !CLI.CommonNodeArgs -- ^ Common CLI args for nodes
  , _srPeers          :: ![NodeId]
  , _srUiMode         :: !ScriptRunnerUIMode
  } deriving Show

makeLenses ''ScriptRunnerOptions
