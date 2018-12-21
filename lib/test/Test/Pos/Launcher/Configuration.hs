module Test.Pos.Launcher.Configuration (tests) where

import           Hedgehog (Property)
import           Universum
import qualified Hedgehog as H

import           Test.Pos.Util.Golden (eachOf)
import           Test.Pos.Util.Tripping (discoverRoundTrip, roundTripsAesonShow)
import           Test.Pos.Launcher.Gen (genConfiguration, genUpdate)

roundTripConfiguration :: Property
roundTripConfiguration =
    eachOf 1000 genConfiguration roundTripsAesonShow

-- move roundTripUpdateConfiguration to chain project
roundTripUpdateConfiguration :: Property
roundTripUpdateConfiguration =
    eachOf 1000 genUpdate roundTripsAesonShow

tests :: IO Bool
tests = H.checkParallel $$discoverRoundTrip
