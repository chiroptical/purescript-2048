module Component.Hello where

import Prelude
-- halogen
import Halogen as H
import Halogen.HTML as HH

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: identity
    , render: const $ HH.h1_ [ HH.text "Component.Hello Component" ]
    , eval: H.mkEval H.defaultEval
    }
