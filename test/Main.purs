module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.QuickCheck (quickCheck)
import Data.Maybe (Maybe)
import Data.Array (catMaybes, length)
import Test.QuickCheck.Gen (Gen, arrayOf)
import Test.QuickCheck.Arbitrary (arbitrary)
import Component.Board (Four(..), verticalMirror, transpose, rotateRight, rotateLeft, foldRight)
import Data.Foldable (sum)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Board Rotations" do
          it "Should mirror column Zero to Three" do
            verticalMirror { row: Zero, column: Zero } `shouldEqual` { row: Zero, column: Three }
          it "Should mirror column One to Two" do
            verticalMirror { row: Zero, column: One } `shouldEqual` { row: Zero, column: Two }
          it "Should modify the row" do
            liftEffect
              $ quickCheck \r -> verticalMirror { row: r, column: Zero } /= { row: r, column: Zero }
          it "Should transpose row and column" do
            liftEffect
              $ quickCheck \r c -> transpose { row: r, column: c } == { row: c, column: r }
          it "Should transpose twice and return itself" do
            liftEffect
              $ quickCheck \r c -> (transpose <<< transpose) { row: r, column: c } == { row: r, column: c }
          it "Should rotate right from Zero,Zero to Zero,Three" do
            rotateRight { row: Zero, column: Zero } `shouldEqual` { row: Zero, column: Three }
          it "Should rotate right from One,One to One,Two" do
            rotateRight { row: One, column: One } `shouldEqual` { row: One, column: Two }
          it "Should rotate right from One,Three to Three,Two" do
            rotateRight { row: One, column: Three } `shouldEqual` { row: Three, column: Two }
          it "Should rotate left from Zero,Zero to Three,Zero" do
            rotateLeft { row: Zero, column: Zero } `shouldEqual` { row: Three, column: Zero }
          it "Should rotate left from One,One to Two,One" do
            rotateLeft { row: One, column: One } `shouldEqual` { row: Two, column: One }
          it "Should rotate left from One,Three to Zero,One" do
            rotateLeft { row: One, column: Three } `shouldEqual` { row: Zero, column: One }
          it "Should rotate right and modify location" do
            liftEffect
              $ quickCheck \r c -> rotateRight { row: r, column: c } /= { row: r, column: c }
          it "Should rotate left and modify location" do
            liftEffect
              $ quickCheck \r c -> rotateLeft { row: r, column: c } /= { row: r, column: c }
          it "Should rotate left then right and return identity" do
            liftEffect
              $ quickCheck \r c -> (rotateLeft <<< rotateRight) { row: r, column: c } == { row: r, column: c }
          it "Should rotate right then left and return identity" do
            liftEffect
              $ quickCheck \r c -> (rotateRight <<< rotateLeft) { row: r, column: c } == { row: r, column: c }
        describe "Folding Function" do
          -- TODO: Write some more specific unit tests for issues
          it "Should sum to the same number" do
            let
              sumJusts = sum <<< catMaybes
            liftEffect
              $ quickCheck \r -> sumJusts (foldRight r) == sumJusts r
          it "Should count Justs and stay same or decrease" do
            let
              countJusts = length <<< catMaybes
            liftEffect
              $ quickCheck \r -> countJusts (foldRight r) <= countJusts r

row :: Gen (Array (Maybe Int))
row = arrayOf arbitrary
