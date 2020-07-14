module Component.Board where

import Prelude

import Data.Array
import CSS (black, border, px, solid) as CSS
import Capability.Random (class Random, shuffleArray)
import Component.Tile as Tile
import Control.Monad.State.Class (get, put)
import Data.Array.NonEmpty as NE
import Data.Const (Const)
import Data.Foldable (foldr, sum)
import Data.Four (Four(..))
import Data.Location (Location, locations, initialLocations)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style) as CSS
import Halogen.HTML.Events as HE
import Halogen.Query.EventSource as ES
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

type State
  = { board :: Map Location Int
    }

data Action
  = Init
  | HandleKey H.SubscriptionId KeyboardEvent
  | ResetGame

type NoQuery
  = Const Void

type ChildSlot
  = H.Slot NoQuery Unit Location

type ChildSlots
  = (tile :: ChildSlot)

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

mkState :: Tuple Location Location -> State
mkState (Tuple loc0 loc1) = { board: Map.insert loc1 2 (Map.singleton loc0 2) }

component :: forall m. MonadAff m => Random m => H.Component HH.HTML NoQuery (Tuple Location Location) Unit m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval (H.defaultEval { handleAction = handleAction, initialize = Just Init })
    }
  where
  initialState :: Tuple Location Location -> State
  initialState = mkState

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    let
      mkTileSlot' = mkTileSlot state
    in
      HH.div_
        [ HH.table
            [ CSS.style do
                CSS.border CSS.solid (CSS.px 2.0) CSS.black
            ]
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
        , HH.button 
            [ HE.onClick \_ -> Just ResetGame ]
            [ HH.text "Reset Game" ]
        ]

  handleAction :: Action -> H.HalogenM State Action ChildSlots Unit m Unit
  handleAction = case _ of
    Init -> do
      document <- H.liftEffect $ Web.document =<< Web.window
      H.subscribe' \sid ->
        ES.eventListenerEventSource
          KET.keyup
          (HTMLDocument.toEventTarget document)
          (map (HandleKey sid) <<< KE.fromEvent)
    HandleKey sid ev -> do
      case KE.key ev of
        "ArrowRight" -> handleAndFillEmptySlot handleRightArrow
        "l" -> handleAndFillEmptySlot handleRightArrow
        "ArrowLeft" -> handleAndFillEmptySlot handleLeftArrow
        "h" -> handleAndFillEmptySlot handleLeftArrow
        "ArrowUp" -> handleAndFillEmptySlot handleUpArrow
        "k" -> handleAndFillEmptySlot handleUpArrow
        "ArrowDown" -> handleAndFillEmptySlot handleDownArrow
        "j" -> handleAndFillEmptySlot handleDownArrow
        "n" -> resetState
        _ -> pure unit
    ResetGame -> resetState

resetState :: forall m. MonadAff m => Random m => H.HalogenM State Action ChildSlots Unit m Unit
resetState = mkState <$> initialLocations >>= put

handleAndFillEmptySlot :: forall m. MonadAff m => Random m => (State -> State) -> H.HalogenM State Action ChildSlots Unit m Unit
handleAndFillEmptySlot handler = do
  { board: mli } <- handler <$> get
  emptySlot <- randomEmptySlot mli
  put $ { board: Map.insert emptySlot 2 mli }

randomEmptySlot :: forall m. Random m => Map Location Int -> m Location
randomEmptySlot mli = do
  locs <- shuffleArray emptySlots
  let
    empty = { row: Zero, column: Zero }
  pure $ fromMaybe empty (head locs)
  where
  emptySlots :: Array Location
  emptySlots =
    foldr
      ( \key acc -> case Map.lookup key mli of
          Just _ -> acc
          Nothing -> key : acc
      )
      []
      locations

verticalMirror :: Location -> Location
verticalMirror loc@{ row: _, column: c } = case c of
  Zero -> loc { column = Three }
  One -> loc { column = Two }
  Two -> loc { column = One }
  Three -> loc { column = Zero }

transpose :: Location -> Location
transpose { row, column } = { row: column, column: row }

rotateLeft :: Location -> Location
rotateLeft = transpose <<< verticalMirror

rotateRight :: Location -> Location
rotateRight = verticalMirror <<< transpose

handleLeftArrow :: State -> State
handleLeftArrow { board: b } =
  let
    { board: flippedBoard } = handleRightArrow { board: modifyBoard verticalMirror b }
  in
    { board: modifyBoard verticalMirror flippedBoard }

handleUpArrow :: State -> State
handleUpArrow { board: b } =
  let
    { board: rotatedBoard } = handleRightArrow { board: modifyBoard rotateRight b }
  in
    { board: modifyBoard rotateLeft rotatedBoard }

handleDownArrow :: State -> State
handleDownArrow { board: b } =
  let
    { board: rotatedBoard } = handleRightArrow { board: modifyBoard rotateLeft b }
  in
    { board: modifyBoard rotateRight rotatedBoard }

modifyBoard :: (Location -> Location) -> Map Location Int -> Map Location Int
modifyBoard modify board =
  foldr
    ( \k acc -> case Map.lookup k board of
        Nothing -> acc
        Just x -> Map.insert (modify k) x acc
    )
    Map.empty
    $ Map.keys board

handleRightArrow :: State -> State
handleRightArrow { board: b } =
  { board:
    Map.unions
      [ mkBoard firstRow
      , mkBoard secondRow
      , mkBoard thirdRow
      , mkBoard fourthRow
      ]
  }
  where
  mkBoard :: Array { key :: Location, value :: Maybe Int } -> Map Location Int
  mkBoard row =
    foldr
      ( \{ key: location, value: val } b' -> case val of
          Nothing -> b'
          Just x -> Map.insert location x b'
      )
      Map.empty
      row

  firstRow = mkRow Zero

  secondRow = mkRow One

  thirdRow = mkRow Two

  fourthRow = mkRow Three

  mkRow :: Four -> Array { key :: Location, value :: Maybe Int }
  mkRow four =
    let
      keys =
        [ { row: four, column: Zero }
        , { row: four, column: One }
        , { row: four, column: Two }
        , { row: four, column: Three }
        ]

      values = foldRowRight $ flip Map.lookup b <$> keys
    in
      zipWith { key: _, value: _ } keys values

fillNothingFromLeft :: forall a. Int -> Array (Maybe a) -> Array (Maybe a)
fillNothingFromLeft n xs = replicate (n - length xs) Nothing <> xs

chunksOf :: Int -> Array Int -> Array (Array Int)
chunksOf n = case _ of
  [] -> []
  xs -> take n xs : chunksOf n (drop n xs)

foldTogether :: Array Int -> Array Int
foldTogether xs = group xs >>= (NE.toArray >>> reverse >>> chunksOf 2 >>> map sum >>> reverse)

foldRowRight :: Array (Maybe Int) -> Array (Maybe Int)
foldRowRight row =
  row
    # catMaybes
    # case _ of
        [] -> fillNothingFromLeft 4 []
        xs ->
          xs
            # foldTogether
            # map Just
            # fillNothingFromLeft 4
