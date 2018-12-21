{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Execution mode used in Auxx.

module PocMode
       (
       -- * Mode, context, etc.
         AuxxContext (..)
       , PocMode
       , MonadPocMode
       , acScriptOptions

       -- * Helpers
       , realModeToAuxx
       , writeBrickChan
       , nodeHandles
       ) where

import           Universum

import           Control.Lens (lens, makeLenses)
import           Control.Monad.Reader (withReaderT)
import           Control.Monad.Trans.Resource (transResourceT)
import           Data.Conduit (transPipe)

import           Brick.BChan (BChan, writeBChan)
import           BrickUITypes (CustomEvent)
import           Pos.Chain.Block (HasSlogContext (slogContext),
                     HasSlogGState (slogGState))
import           Pos.Chain.Ssc (HasSscContext (sscContext))
import           Pos.Client.KeyStorage (MonadKeys (modifySecret),
                     MonadKeysRead (getSecret), getSecretDefault,
                     modifySecretDefault)
import           Pos.Client.Txp.Balances
                     (MonadBalances (getBalance, getOwnUtxos),
                     getBalanceFromUtxo, getOwnUtxosGenesis)
import           Pos.Client.Txp.History
                     (MonadTxHistory (getBlockHistory, getLocalHistory, saveTx),
                     getBlockHistoryDefault, getLocalHistoryDefault,
                     saveTxDefault)
import           Pos.Context (HasNodeContext (nodeContext))
import           Pos.Core (HasPrimaryKey (primaryKey))
import           Pos.Core.JsonLog (CanJsonLog (jsonLog))
import           Pos.Core.Reporting (HasMisbehaviorMetrics (misbehaviorMetrics),
                     MonadReporting (report))
import           Pos.Core.Slotting
                     (HasSlottingVar (slottingTimestamp, slottingVar),
                     MonadSlotsData)
import           Pos.DB (MonadGState (gsAdoptedBVData))
import           Pos.DB.Block (MonadBListener (onApplyBlocks, onRollbackBlocks))
import           Pos.DB.Class
                     (MonadDB (dbDelete, dbPut, dbPutSerBlunds, dbWriteBatch),
                     MonadDBRead (dbGet, dbGetSerBlock, dbGetSerBlund, dbGetSerUndo, dbIterSource))
import           Pos.DB.Txp (MempoolExt,
                     MonadTxpLocal (txpNormalize, txpProcessTx), txNormalize,
                     txProcessTransaction)
import           Pos.Infra.Network.Types (HasNodeType (getNodeType),
                     NodeType (NodeEdge))
import           Pos.Infra.Shutdown (HasShutdownContext (shutdownContext))
import           Pos.Infra.Slotting.Class (MonadSlots (currentTimeSlotting, getCurrentSlot, getCurrentSlotBlocking, getCurrentSlotInaccurate))
import           Pos.Launcher (HasConfigurations)
import           Pos.Util (HasLens (lensOf))
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Util.LoggerName (HasLoggerName' (loggerName))
import           Pos.Util.UserSecret (HasUserSecret (userSecret))
import           Pos.Util.Wlog (HasLoggerName (askLoggerName, modifyLoggerName))
import           Pos.WorkMode (EmptyMempoolExt, RealMode, RealModeContext)

import Types (NodeHandle, NodeType, ScriptRunnerOptions)

type PocMode = ReaderT AuxxContext IO

class (m ~ PocMode, HasConfigurations, HasCompileInfo) => MonadPocMode m
instance (HasConfigurations, HasCompileInfo) => MonadPocMode PocMode

data AuxxContext = AuxxContext
    { _acRealModeContext :: !(RealModeContext EmptyMempoolExt)
    , _acEventChan       :: !(BChan CustomEvent)
    , _acNodeHandles     :: !(TVar (Map (Types.NodeType,Integer) NodeHandle))
    , _acScriptOptions   :: !(ScriptRunnerOptions)
    }

makeLenses ''AuxxContext

writeBrickChan :: CustomEvent -> PocMode ()
writeBrickChan event = do
  chan <- view acEventChan
  liftIO $ writeBChan chan event

nodeHandles :: PocMode (TVar (Map (Types.NodeType,Integer) NodeHandle))
nodeHandles = view acNodeHandles

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Turn 'RealMode' action into 'PocMode' action.
realModeToAuxx :: RealMode EmptyMempoolExt a -> PocMode a
realModeToAuxx = withReaderT _acRealModeContext

----------------------------------------------------------------------------
-- Boilerplate instances
----------------------------------------------------------------------------

instance HasSscContext AuxxContext where
    sscContext = acRealModeContext . sscContext

instance HasPrimaryKey AuxxContext where
    primaryKey = acRealModeContext . primaryKey

-- | Ignore reports.
-- FIXME it's a bad sign that we even need this instance.
-- The pieces of the software which the block generator uses should never
-- even try to report.
instance MonadReporting PocMode where
    report _ = pure ()

-- | Ignore reports.
-- FIXME it's a bad sign that we even need this instance.
instance HasMisbehaviorMetrics AuxxContext where
    misbehaviorMetrics = lens (const Nothing) const

instance HasUserSecret AuxxContext where
    userSecret = acRealModeContext . userSecret

instance HasShutdownContext AuxxContext where
    shutdownContext = acRealModeContext . shutdownContext

instance HasNodeContext AuxxContext where
    nodeContext = acRealModeContext . nodeContext

instance HasSlottingVar AuxxContext where
    slottingTimestamp = acRealModeContext . slottingTimestamp
    slottingVar = acRealModeContext . slottingVar

instance HasNodeType AuxxContext where
    getNodeType _ = NodeEdge

instance {-# OVERLAPPABLE #-}
    HasLens tag (RealModeContext EmptyMempoolExt) r =>
    HasLens tag AuxxContext r
  where
    lensOf = acRealModeContext . lensOf @tag

instance HasLoggerName' AuxxContext where
    loggerName = acRealModeContext . loggerName

instance HasSlogContext AuxxContext where
    slogContext = acRealModeContext . slogContext

instance HasSlogGState AuxxContext where
    slogGState = acRealModeContext . slogGState

instance MonadSlotsData ctx PocMode => MonadSlots ctx PocMode where
    getCurrentSlot = realModeToAuxx . getCurrentSlot
    getCurrentSlotBlocking = realModeToAuxx . getCurrentSlotBlocking
    getCurrentSlotInaccurate = realModeToAuxx . getCurrentSlotInaccurate
    currentTimeSlotting = realModeToAuxx currentTimeSlotting

instance {-# OVERLAPPING #-} HasLoggerName PocMode where
    askLoggerName = realModeToAuxx askLoggerName
    modifyLoggerName f action = do
        auxxCtx <- ask
        let auxxToRealMode :: PocMode a -> RealMode EmptyMempoolExt a
            auxxToRealMode = withReaderT (\realCtx -> set acRealModeContext realCtx auxxCtx)
        realModeToAuxx $ modifyLoggerName f $ auxxToRealMode action

instance {-# OVERLAPPING #-} CanJsonLog PocMode where
    jsonLog = realModeToAuxx ... jsonLog

instance MonadDBRead PocMode where
    dbGet = realModeToAuxx ... dbGet
    dbIterSource tag p =
        transPipe (transResourceT realModeToAuxx) (dbIterSource tag p)
    dbGetSerBlock = realModeToAuxx ... dbGetSerBlock
    dbGetSerUndo = realModeToAuxx ... dbGetSerUndo
    dbGetSerBlund = realModeToAuxx ... dbGetSerBlund

instance MonadDB PocMode where
    dbPut = realModeToAuxx ... dbPut
    dbWriteBatch = realModeToAuxx ... dbWriteBatch
    dbDelete = realModeToAuxx ... dbDelete
    dbPutSerBlunds = realModeToAuxx ... dbPutSerBlunds

instance MonadGState PocMode where
    gsAdoptedBVData = realModeToAuxx ... gsAdoptedBVData

instance MonadBListener PocMode where
    onApplyBlocks = realModeToAuxx ... onApplyBlocks
    onRollbackBlocks = realModeToAuxx ... onRollbackBlocks

instance MonadBalances PocMode where
    getOwnUtxos genesisData addrs = getOwnUtxosGenesis genesisData addrs
    getBalance = getBalanceFromUtxo

instance MonadTxHistory PocMode where
    getBlockHistory = getBlockHistoryDefault
    getLocalHistory = getLocalHistoryDefault
    saveTx = saveTxDefault

instance MonadKeysRead PocMode where
    getSecret = getSecretDefault

instance MonadKeys PocMode where
    modifySecret = modifySecretDefault

type instance MempoolExt PocMode = EmptyMempoolExt

instance MonadTxpLocal PocMode where
    txpNormalize pm = withReaderT _acRealModeContext . txNormalize pm
    txpProcessTx genesisConfig txpConfig = withReaderT _acRealModeContext . txProcessTransaction genesisConfig txpConfig

