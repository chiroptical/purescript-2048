module Data.Location where

import Prelude
import Capability.Random (class Random, shuffleArray)
import Data.Array (filter, head)
import Data.Four (Four(..), randomFour, fours)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)

type Location
  = { row :: Four, column :: Four }

empty :: Tuple Location Location
empty = Tuple { row: Zero, column: Zero } { row: One, column: One }

possibleStartingLocations :: Array (Tuple Location Location)
possibleStartingLocations =
  Tuple <$> locations <*> locations
    # filter (\(Tuple x y) -> x /= y)

initialLocations :: forall m. Random m => m (Tuple Location Location)
initialLocations = do
  locs <- shuffleArray possibleStartingLocations
  pure $ fromMaybe empty (head locs)

randomLocation :: Effect Location
randomLocation = { row: _, column: _ } <$> randomFour <*> randomFour

locations :: Array Location
locations = { row: _, column: _ } <$> fours <*> fours
