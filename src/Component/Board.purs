module Component.Board where

import Prelude
import Component.Tile as Tile
import Control.Monad.State.Class (modify_)
import Data.Array ((:), reverse, findIndex, index, snoc, updateAt, insertAt, zipWith)
import Data.Const (Const)
import Data.Enum (class Enum, toEnum, fromEnum, class BoundedEnum)
import Data.Foldable (foldl, foldr)
import Data.Either as E
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericTop, genericBottom)
import Data.Generic.Rep.Enum (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Set (Set)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff)
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
import CSS as CSS
import Halogen.HTML.CSS as CSS

type State
  = { board :: Board2048
    }

type Board2048
  = Map Location Int

type Location
  = { row :: Four, column :: Four }

-- TODO:
-- 1. This could be simplified using lenses?
-- 2. Should occasionally give 4, waiting on 2048 Type
initialLocations :: Effect Input
initialLocations = do
  loc@{ fst: one, snd: two@{ row: r, column: c } } <- { fst: _, snd: _ } <$> randomLocation <*> randomLocation
  if one == two then
    pure loc { snd = { row: fourWrappingSucc r, column: fourWrappingSucc c } }
  else
    pure loc

data Four
  = Zero
  | One
  | Two
  | Three

fourWrappingSucc :: Four -> Four
fourWrappingSucc = case _ of
  Zero -> One
  One -> Two
  Two -> Three
  Three -> Zero

fourWrappingPred :: Four -> Four
fourWrappingPred = case _ of
  Zero -> Three
  One -> Zero
  Two -> One
  Three -> Two

type Input
  = { fst :: Location, snd :: Location }

derive instance genericFour :: Generic Four _

derive instance eqFour :: Eq Four

derive instance ordFour :: Ord Four

instance showFour :: Show Four where
  show = genericShow

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
  int <- randomREff (fromEnum (bottom :: Four)) (fromEnum (top :: Four))
  pure $ maybe Zero identity $ toEnum int

randomLocation :: Effect Location
randomLocation = { row: _, column: _ } <$> randomFour <*> randomFour

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
        ]

  -- TODO: Handle ArrowUp/ArrowDown using rotations and handleRightArrow/handleLeftArrow
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
      loc <- H.liftEffect randomLocation
      case KE.key ev of
        "ArrowRight" -> modify_ (insertIntoBoard loc <<< handleRightArrow)
        "ArrowLeft" -> modify_ (insertIntoBoard loc <<< handleLeftArrow)
        "ArrowUp" -> modify_ (insertIntoBoard loc <<< handleUpArrow)
        "ArrowDown" -> modify_ (insertIntoBoard loc <<< handleDownArrow)
        _ -> pure unit

insertIntoBoard :: Location -> State -> State
insertIntoBoard loc { board: b } =
  { board:
      Map.alter
        ( case _ of
            Nothing -> Just 2
            x -> x
        )
        loc
        b
  }

mirrorColumnwise :: Location -> Location
mirrorColumnwise loc@{ row: _, column: c } = case c of
  Zero -> loc { column = Three }
  One -> loc { column = Two }
  Two -> loc { column = One }
  Three -> loc { column = Zero }

handleLeftArrow :: State -> State
handleLeftArrow { board: b } =
  let
    { board: flippedBoard } = handleRightArrow { board: mirrorBoardColumnwise b }
  in
    { board: mirrorBoardColumnwise flippedBoard }

handleUpArrow :: State -> State
handleUpArrow { board: b } =
  let
    { board: rotatedBoard } = handleRightArrow { board: boardRotation b Right }
  in
    { board: boardRotation rotatedBoard Left }

handleDownArrow :: State -> State
handleDownArrow { board: b } =
  let
    { board: rotatedBoard } = handleRightArrow { board: boardRotation b Left }
  in
    { board: boardRotation rotatedBoard Right }

mirrorBoardColumnwise :: Board2048 -> Board2048
mirrorBoardColumnwise board =
  foldr
    ( \k acc -> case Map.lookup k board of
        Nothing -> acc
        Just x -> Map.insert (mirrorColumnwise k) x acc
    )
    Map.empty
    $ Map.keys board

data Rotation
  = Left
  | Right

rightRotation :: Location -> Location
rightRotation = case _ of
  { row: r@One, column: c@One } -> { row: invertFour r, column: c }
  { row: r@One, column: c@Two } -> { row: invertFour r, column: c }
  { row: r@Two, column: c@One } -> { row: invertFour r, column: c }
  { row: r@Two, column: c@Two } -> { row: invertFour r, column: c }
  { row: r, column: c } -> { row: c, column: invertFour r }

leftRotation :: Location -> Location
leftRotation = case _ of
  { row: r@One, column: c@One } -> { row: r, column: invertFour c }
  { row: r@One, column: c@Two } -> { row: r, column: invertFour c }
  { row: r@Two, column: c@One } -> { row: r, column: invertFour c }
  { row: r@Two, column: c@Two } -> { row: r, column: invertFour c }
  { row: r, column: c } -> { row: invertFour c, column: r }

invertFour :: Four -> Four
invertFour = case _ of
  Zero -> Three
  One -> Two
  Two -> One
  Three -> Zero

boardRotation :: Board2048 -> Rotation -> Board2048
boardRotation board = case _ of
  Right -> rotate rightRotation locations
  Left -> rotate leftRotation locations
  where
  locations = Map.keys board

  rotate :: (Location -> Location) -> (Set Location -> Board2048)
  rotate f =
    foldr
      ( \k acc -> case Map.lookup k board of
          Nothing -> acc
          Just x -> Map.insert (f k) x acc
      )
      Map.empty

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
  mkBoard :: Array { key :: Location, value :: Maybe Int } -> Board2048
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

      values = foldRight $ flip Map.lookup b <$> keys
    in
      zipWith { key: _, value: _ } keys values

data Combined x
  = Uncombined x
  | Combined x

-- TODO:
-- 1. Can we simplify using the Maybe Monad (or catMaybes, suggestion from paluh__)
-- 2. Property Tests, e.g.
--    - Sum of `Just` elements should never change
--    - Number of `Just` should always decrease or stay the same
foldRight :: Array (Maybe Int) -> Array (Maybe Int)
foldRight xs =
  let
    xs' = map (map E.Left) xs

    undoEither :: forall a. E.Either a a -> a
    undoEither (E.Left x) = x

    undoEither (E.Right x) = x
  in
    map (map undoEither)
      ( foldl
          ( \acc x -> case x of
              Nothing -> x : acc
              r@(Just (E.Right x')) -> snoc acc r
              r@(Just (E.Left x')) ->
                let
                  firstJustIdx :: Maybe Int
                  firstJustIdx = findIndex isJust acc

                  firstJust :: Maybe (E.Either Int Int)
                  firstJust = firstJustIdx >>= index acc >>= identity
                in
                  case { idx: firstJustIdx, value: firstJust } of
                    { idx: Nothing, value: _ } -> snoc acc r
                    { idx: _, value: Nothing } -> snoc acc r
                    { idx: Just idx, value: Just (E.Right _) } -> maybe [] identity $ insertAt idx r acc
                    { idx: Just idx, value: Just (E.Left x'') } ->
                      maybe [] identity
                        ( if x' == x'' then
                            (Nothing : _) <$> updateAt idx (Just <<< E.Right $ x' + x'') acc
                          else
                            insertAt idx r acc
                        )
          )
          ([] :: Array (Maybe (E.Either Int Int)))
          (reverse xs')
      )
