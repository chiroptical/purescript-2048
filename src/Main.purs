module Main where

import Prelude

import Component.Board as Board
import Data.Location (initialLocations)
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import AppM (runAppM)

main :: Effect Unit
main = do
  HA.runHalogenAff do
    locations <- runAppM initialLocations
    body <- HA.awaitBody

    let app = H.hoist runAppM Board.component
    root <- runUI app locations body
    pure unit
