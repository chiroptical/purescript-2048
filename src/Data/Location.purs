module Data.Location where

import Prelude
import Data.Array (filter, head, concat)
import Data.Four (Four(..), randomFour, fours)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Test.QuickCheck.Gen (randomSample', shuffle)
import Data.Maybe (fromMaybe)

type Location
  = { row :: Four, column :: Four }

possibleStartingLocations :: Array (Tuple Location Location)
possibleStartingLocations =
  Tuple <$> locations <*> locations
    # filter (\(Tuple x y) -> x /= y)

initialLocations :: Effect (Tuple Location Location)
initialLocations = do
  locs <- concat <$> (randomSample' 1 $ shuffle possibleStartingLocations)
  let
    empty = Tuple { row: Zero, column: Zero } { row: One, column: One }
  pure $ fromMaybe empty (head locs)

randomLocation :: Effect Location
randomLocation = { row: _, column: _ } <$> randomFour <*> randomFour

locations :: Array Location
locations = { row: _, column: _ } <$> fours <*> fours
