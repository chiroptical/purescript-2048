module Component.Board where

import Prelude
import Component.Tile as Tile
import Data.Const (Const)
import Data.Enum (class Enum, toEnum, class BoundedEnum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Enum (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep.Bounded (genericTop, genericBottom)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.EventSource as ES
import Random.PseudoRandom (randomREff)
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

type State
  = { board :: Board2048
    }

type Board2048
  = Map Location Int

type Location
  = { row :: Four, column :: Four }

data Four
  = Zero
  | One
  | Two
  | Three

type Input
  = { fst :: Location, snd :: Location }

derive instance genericFour :: Generic Four _

derive instance eqFour :: Eq Four

derive instance ordFour :: Ord Four

instance enumFour :: Enum Four where
  succ = genericSucc
  pred = genericPred

instance boundedFour :: Bounded Four where
  top = genericTop
  bottom = genericBottom

instance boundedEnumFour :: BoundedEnum Four where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

randomFour :: Effect Four
randomFour = do
  int <- randomREff 0 3
  pure $ maybe Zero identity $ toEnum int

randomLocation :: Effect Location
randomLocation = { row: _, column: _ } <$> randomFour <*> randomFour

{-- reduceLeft :: Board2048 -> Board2048 --}
{-- reduceLeft = ?a --}
data Action
  = Init
  | HandleKey H.SubscriptionId KeyboardEvent

type NoQuery
  = Const Void

type ChildSlot
  = H.Slot NoQuery Unit Location

type ChildSlots
  = ( tile :: ChildSlot
    )

_tile :: SProxy "tile"
_tile = SProxy

mkTileSlot ::
  forall action m.
  State ->
  Location ->
  HH.HTML
    ( H.ComponentSlot HH.HTML ChildSlots
        action
        m
    )
    m
mkTileSlot state location = HH.slot _tile location Tile.component (Map.lookup location state.board) (\_ -> Nothing)

component :: forall q. H.Component HH.HTML q Input Unit Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval (H.defaultEval { handleAction = handleAction, initialize = Just Init })
    }
  where
  initialState :: Input -> State
  initialState { fst: loc0, snd: loc1 } = { board: Map.insert loc1 2 (Map.singleton loc0 2) }

  render :: State -> H.ComponentHTML Action ChildSlots Aff
  render state =
    let
      mkTileSlot' = mkTileSlot state
    in
      HH.div_
        [ HH.table_
            [ HH.tr_
                [ mkTileSlot' { row: Zero, column: Zero }
                , mkTileSlot' { row: Zero, column: One }
                , mkTileSlot' { row: Zero, column: Two }
                , mkTileSlot' { row: Zero, column: Three }
                ]
            , HH.tr_
                [ mkTileSlot' { row: One, column: Zero }
                , mkTileSlot' { row: One, column: One }
                , mkTileSlot' { row: One, column: Two }
                , mkTileSlot' { row: One, column: Three }
                ]
            , HH.tr_
                [ mkTileSlot' { row: Two, column: Zero }
                , mkTileSlot' { row: Two, column: One }
                , mkTileSlot' { row: Two, column: Two }
                , mkTileSlot' { row: Two, column: Three }
                ]
            , HH.tr_
                [ mkTileSlot' { row: Three, column: Zero }
                , mkTileSlot' { row: Three, column: One }
                , mkTileSlot' { row: Three, column: Two }
                , mkTileSlot' { row: Three, column: Three }
                ]
            ]
        ]

  handleAction :: Action -> H.HalogenM State Action ChildSlots Unit Aff Unit
  handleAction = case _ of
    Init -> do
      document <- H.liftEffect $ Web.document =<< Web.window
      H.subscribe' \sid ->
        ES.eventListenerEventSource
          KET.keyup
          (HTMLDocument.toEventTarget document)
          (map (HandleKey sid) <<< KE.fromEvent)
    HandleKey sid ev -> case KE.key ev of
      "ArrowRight" -> pure unit
      "ArrowLeft" -> do
        pure unit
      "ArrowUp" -> pure unit
      "ArrowDown" -> pure unit
      _ -> H.liftEffect $ Console.log (KE.key ev)
