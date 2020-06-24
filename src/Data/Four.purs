module Data.Four where

import Prelude
import Data.Enum (class Enum, toEnum, fromEnum, class BoundedEnum, enumFromTo)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericTop, genericBottom)
import Data.Generic.Rep.Enum (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep.Show (genericShow)
import Data.Array.NonEmpty as NE
import Data.Foldable (foldr)
import Test.QuickCheck.Gen (oneOf)
import Effect (Effect)
import Random.PseudoRandom (randomREff)
import Data.Maybe (maybe)

data Four
  = Zero
  | One
  | Two
  | Three

instance arbFour :: Arbitrary Four where
  arbitrary = oneOf (map pure (NE.toNonEmpty nonEmptyArr))
    where
    nonEmptyArr :: NE.NonEmptyArray Four
    nonEmptyArr = foldr NE.insert (NE.singleton Zero) [ One, Two, Three ]

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

fours :: Array Four
fours = enumFromTo Zero Three
