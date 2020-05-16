module Component.Tile where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Array (singleton)
-- halogen
import Halogen as H
import Halogen.HTML as HH

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
  render st = HH.td_ (maybe [] (singleton <<< HH.text <<< show) st)

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  HandleInput n -> H.put n
