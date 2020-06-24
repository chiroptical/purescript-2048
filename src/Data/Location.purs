module Data.Location where

import Prelude
import Data.Four (Four, fourWrappingSucc, randomFour)
import Effect (Effect)
import Data.Tuple (Tuple(..))

type Location
  = { row :: Four, column :: Four }

initialLocations :: Effect (Tuple Location Location)
initialLocations = do
  loc@(Tuple one two@{ row: r, column: c }) <- Tuple <$> randomLocation <*> randomLocation
  if one == two then
    pure $ Tuple one { row: fourWrappingSucc r, column: fourWrappingSucc c }
  else
    pure loc

randomLocation :: Effect Location
randomLocation = { row: _, column: _ } <$> randomFour <*> randomFour
