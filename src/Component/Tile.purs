module Component.Tile where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Array (singleton)
-- halogen
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import CSS as CSS

type State
  = Maybe Int

type Input
  = Maybe Int

data Action
  = HandleInput (Maybe Int)

component :: forall q o m. H.Component HH.HTML q (Maybe Int) o m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , receive = Just <<< HandleInput
              }
    }
  where
  render :: forall action. State -> H.ComponentHTML action () m
  render st =
    HH.td
      [ CSS.style do
          CSS.width (CSS.px 20.0)
          CSS.height (CSS.px 20.0)
          CSS.border CSS.solid (CSS.px 1.0) CSS.black
      ]
      (maybe [] (singleton <<< HH.text <<< show) st)

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  HandleInput n -> H.put n
