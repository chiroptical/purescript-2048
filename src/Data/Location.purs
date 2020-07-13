module Data.Location where

import Prelude

import Capability.Random (class Random, randomSample)
import Data.Array (filter, head)
import Data.Const (Const)
import Data.Four (Four(..), randomFour, fours)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Halogen (Slot) as H

type Location
  = { row :: Four, column :: Four }

empty :: Tuple Location Location
empty = Tuple { row: Zero, column: Zero } { row: One, column: One }

type Slot = H.Slot (Const Void) Unit Location

possibleStartingLocations :: Array (Tuple Location Location)
possibleStartingLocations =
  Tuple <$> locations <*> locations
    # filter (\(Tuple x y) -> x /= y)

initialLocations :: forall m. Random m => m (Tuple Location Location)
initialLocations = do
  locs <- randomSample possibleStartingLocations
  pure $ fromMaybe empty (head locs)

randomLocation :: Effect Location
randomLocation = { row: _, column: _ } <$> randomFour <*> randomFour

locations :: Array Location
locations = { row: _, column: _ } <$> fours <*> fours
