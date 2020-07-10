module Component.Board where

import Data.Four (Four(..))
import Data.Array
import Prelude
import CSS (black, border, px, solid) as CSS
import Component.Tile as Tile
import Control.Monad.State.Class (get, put)
import Data.Array.NonEmpty as NE
import Data.Const (Const)
import Data.Foldable (foldr, sum)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.CSS (style) as CSS
import Halogen.Query.EventSource as ES
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Data.Location (Location, locations, initialLocations)
import Data.Tuple (Tuple(..))
import Test.QuickCheck.Gen (randomSample', shuffle)

type State
  = { board :: Map Location Int
    , score :: Int
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
  = ( tile :: ChildSlot
    )

type IncreaseScore
  = { increaseScoreBy :: Int, row :: Array Int }

type IncreaseScoreM
  = { increaseScoreByM :: Int, rowM :: Array (Maybe Int) }

type IncreaseScoreZ
  = { increaseScoreByZ :: Int, rowZ :: Array { key :: Location, value :: Maybe Int } }

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
mkState (Tuple loc0 loc1) =
  { board: Map.insert loc1 2 (Map.singleton loc0 2)
  , score: 0
  }

component :: forall q. H.Component HH.HTML q (Tuple Location Location) Unit Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval (H.defaultEval { handleAction = handleAction, initialize = Just Init })
    }
  where
  initialState :: Tuple Location Location -> State
  initialState = mkState

  render :: State -> H.ComponentHTML Action ChildSlots Aff
  render state =
    let
      mkTileSlot' = mkTileSlot state
    in
      HH.div_
        [ HH.text ("Score: " <> show state.score)
        , HH.table
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

  handleAction :: Action -> H.HalogenM State Action ChildSlots Unit Aff Unit
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

resetState :: H.HalogenM State Action ChildSlots Unit Aff Unit
resetState = (H.liftEffect $ mkState <$> initialLocations) >>= put

handleAndFillEmptySlot :: (State -> State) -> H.HalogenM State Action ChildSlots Unit Aff Unit
handleAndFillEmptySlot handler = do
  { board: mli, score } <- handler <$> get
  emptySlot <- H.liftEffect $ randomEmptySlot mli
  put $ { board: Map.insert emptySlot 2 mli, score }

randomEmptySlot :: Map Location Int -> Effect Location
randomEmptySlot mli = do
  locs <- concat <$> (randomSample' 1 $ shuffle emptySlots)
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
handleLeftArrow { board, score: currScore } =
  let
    { board: flippedBoard, score } = handleRightArrow { board: modifyBoard verticalMirror board, score: currScore }
  in
    { board: modifyBoard verticalMirror flippedBoard, score }

handleUpArrow :: State -> State
handleUpArrow { board, score: currScore } =
  let
    { board: rotatedBoard, score } = handleRightArrow { board: modifyBoard rotateRight board, score: currScore }
  in
    { board: modifyBoard rotateLeft rotatedBoard, score }

handleDownArrow :: State -> State
handleDownArrow { board, score: currScore } =
  let
    { board: rotatedBoard, score: newScore } = handleRightArrow { board: modifyBoard rotateLeft board, score: currScore }
  in
    { board: modifyBoard rotateRight rotatedBoard, score: newScore }

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
handleRightArrow { board, score } =
  { board:
    Map.unions
      [ mkBoard zeroth.rowZ
      , mkBoard first.rowZ
      , mkBoard second.rowZ
      , mkBoard third.rowZ
      ]
  , score: score + zeroth.increaseScoreByZ + first.increaseScoreByZ + second.increaseScoreByZ + third.increaseScoreByZ
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

  zeroth = mkRow Zero

  first = mkRow One

  second = mkRow Two

  third = mkRow Three

  mkRow :: Four -> IncreaseScoreZ
  mkRow four =
    let
      keys =
        [ { row: four, column: Zero }
        , { row: four, column: One }
        , { row: four, column: Two }
        , { row: four, column: Three }
        ]

      { increaseScoreByM, rowM } = foldRowRight $ flip Map.lookup board <$> keys
    in
      { increaseScoreByZ: increaseScoreByM
      , rowZ: zipWith { key: _, value: _ } keys rowM
      }

fillNothingFromLeft :: forall a. Int -> Array (Maybe a) -> Array (Maybe a)
fillNothingFromLeft n xs = replicate (n - length xs) Nothing <> xs

chunksOf :: Int -> Array Int -> Array (Array Int)
chunksOf n = case _ of
  [] -> []
  xs -> take n xs : chunksOf n (drop n xs)

foldTogether :: Array Int -> IncreaseScore
foldTogether xs =
  let
    chunked = group xs >>= (NE.toArray >>> chunksOf 2 >>> reverse)

    increaseScoreBy =
      chunked
        # filter (length >>> (_ > 1))
        # concat
        # sum
  in
    { increaseScoreBy, row: map sum chunked }

foldRowRight :: Array (Maybe Int) -> IncreaseScoreM
foldRowRight row =
  row
    # catMaybes
    # case _ of
        [] -> { increaseScoreByM: 0, rowM: fillNothingFromLeft 4 [] }
        xs ->
          xs
            # foldTogether
            # \{ increaseScoreBy, row: r } -> { increaseScoreByM: increaseScoreBy, rowM: fillNothingFromLeft 4 (map Just r) }
