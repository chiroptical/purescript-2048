module Main where

import Prelude
import Component.Board as Board
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = do
  locations <- { fst: _, snd: _ } <$> Board.randomLocation <*> Board.randomLocation
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI Board.component locations body
