{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Universum

import           Data.Coerce (coerce)
import           System.Environment (lookupEnv)
import           System.FilePath (takeDirectory, (</>))

import           Pos.Chain.Block (HeaderHash, blockHeaderHash)
import           Pos.Chain.Genesis (GenesisHash (..), StaticConfig (..))
import           Pos.Core (EpochIndex (..), EpochOrSlot (..),
                     LocalSlotIndex (..), SlotId (..), getEpochOrSlot)
import           Pos.DB.Block (dbGetSerBlockRealDefault,
                     dbGetSerBlundRealDefault, dbGetSerUndoRealDefault,
                     dbPutSerBlundsRealDefault, getFirstGenesisBlockHash)
import           Pos.DB.BlockIndex (getHeader, getTipHeader)
import           Pos.DB.Class (MonadDB (..), MonadDBRead (..), dbGetSerBlock)
import           Pos.DB.Rocks.Functions (closeNodeDBs, dbDeleteDefault,
                     dbGetDefault, dbIterSourceDefault, dbPutDefault,
                     dbWriteBatchDefault, openNodeDBs)
import           Pos.DB.Rocks.Types (NodeDBs (..), getBlockIndexDB)
import           Pos.Launcher.Configuration (Configuration (..))
import           Pos.Util.Config (parseYamlConfig)

-- Currently running this against a mainnet cardano-node's config and database using:
--
--  CARDANO_PATH=.local/share/Daedalus/mainnet/configuration.yaml \
--      CARDANO_KEY=mainnet_wallet_linux64 \
--      ./db-explore


main :: IO ()
main = do
    cfgpath <- fromMaybe (error "Environment variable CARDANO_PATH not defined.")
                <$> lookupEnv "CARDANO_PATH"
    cfgkey <- fromMaybe (error "Environment variable CARDANO_KEY not defined.")
                <$> lookupEnv "CARDANO_KEY"

    genesisHash <- extractGenesisHash <$> parseYamlConfig (fromString cfgpath) (fromString cfgkey)

    bracket (openNodeDBs False $ takeDirectory cfgpath </> "DB") closeNodeDBs $ \ nbds ->
        runReaderT (exploreDB genesisHash) nbds

extractGenesisHash :: Configuration -> GenesisHash
extractGenesisHash sc =
    case ccGenesis sc of
        GCSpec _ -> error "extractGenesisHash: Unexpected GCSpec constructor"
        GCSrc _ hr -> GenesisHash $ coerce hr

type ExploreDB = ReaderT NodeDBs IO

-- Currently only needed for `getBlockIndexDB` in `exploreDB`.
instance MonadDBRead ExploreDB where
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault
    dbGetSerBlock = dbGetSerBlockRealDefault
    dbGetSerUndo = dbGetSerUndoRealDefault
    dbGetSerBlund = dbGetSerBlundRealDefault

instance MonadDB ExploreDB where
    dbPut = dbPutDefault
    dbWriteBatch = dbWriteBatchDefault
    dbDelete = dbDeleteDefault
    dbPutSerBlunds = dbPutSerBlundsRealDefault


exploreDB
    :: (MonadCatch m, MonadDB m, MonadMask m, MonadReader NodeDBs m, MonadIO m)
    => GenesisHash -> m ()
exploreDB genesisHash = do
    liftIO $ putStrLn ("Database ok!" :: Text)
    _blocksDB <- getBlockIndexDB
    liftIO $ putStrLn ("Have blockDB" :: Text)

    mtip <- getHeaderEpochOrSlot =<< fmap blockHeaderHash getTipHeader
    printEpochOrSlot "Tip" mtip

    gen <- getFirstGenesisBlockHash genesisHash
    mGenEos <- getHeaderEpochOrSlot gen
    printEpochOrSlot "Genesis" mGenEos


-- -------------------------------------------------------------------------------------------------

getHeaderEpochOrSlot :: MonadDBRead m => HeaderHash -> m (Maybe EpochOrSlot)
getHeaderEpochOrSlot hh =
    getEpochOrSlot <<$>> getHeader hh


printEpochOrSlot :: MonadIO m => String -> Maybe EpochOrSlot -> m ()
printEpochOrSlot s meos =
    liftIO . putStrLn $
        case meos of
              Just eos -> renderEpochOrSlot s eos
              Nothing  -> "No " ++ s ++ " block"

renderEpochOrSlot :: String -> EpochOrSlot -> String
renderEpochOrSlot s eos =
    case unEpochOrSlot eos of
        Right sid -> s ++ " (epoch, slot): " ++ show (getEpochIndex $ siEpoch sid, getSlotIndex $ siSlot sid)
        Left eid -> s ++ " epoch: " ++ show (getEpochIndex eid)
