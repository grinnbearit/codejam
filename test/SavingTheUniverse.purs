module Test.SavingTheUniverse where

import Prelude

import Data.Array (fromFoldable)
import Data.List.Types (List)
import Data.Maybe (Maybe(..))
import Effect (Effect)
-- Import the functions and types we need to test
import SavingTheUniverse (Case(..), formatResults, parseInput, solveCase)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- Our test suite's main entry point
main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec

-- Our main spec, which we now build up
spec :: Spec Unit
spec = do

  -- NEW: Tests for parseInput
  describe "SavingTheUniverse.parseInput" do

    it "parses a simple case correctly" do
      -- Based on your parse-single-case test
      let
        input = "1\n5\nA\nB\nC\nD\nE\n2\nA\nB"
        expected = Just
                   [ Case { engines: [ "A", "B", "C", "D", "E" ]
                          , queries: [ "A", "B" ]
                          }
                   ]

      -- We must convert the List to an Array to compare
      -- with our `expected` (Array) value.
      let actual = fromFoldable <$> parseInput input

      actual `shouldEqual` expected

    it "parses multiple cases correctly" do
      -- Based on your parse-input-string test
      let
        input = "2\n5\nA\nB\nC\nD\nE\n2\nA\nB\n2\nX\nY\n1\nX"
        expected = Just
          [ Case { engines: [ "A", "B", "C", "D", "E" ]
                 , queries: [ "A", "B" ]
                 }
          , Case { engines: [ "X", "Y" ]
                 , queries: [ "X" ]
                 }
          ]

      let actual = fromFoldable <$> parseInput input
      actual `shouldEqual` expected

    it "returns Nothing for badly formed case count" do
      let input = "two\n5\nA\nB\nC\nD\nE\n2\nA\nB\n"
      parseInput input `shouldEqual` (Nothing :: Maybe (List Case))

    it "returns Nothing for badly formed S" do
      let input = "1\nfive\nA\nB\nC\nD\nE\n2\nA\nB\n"
      parseInput input `shouldEqual` (Nothing :: Maybe (List Case))

    it "returns Nothing for badly formed Q" do
      let input = "1\n5\nA\nB\nC\nD\nE\ntwo\nA\nB\n"
      parseInput input `shouldEqual` (Nothing :: Maybe (List Case))

  -- Tests for our core logic
  describe "SavingTheUniverse.solveCase" do

    it "solves sample case #1" do
      let
        case1 :: Case
        case1 = Case
          { engines: [ "Yeehaw", "NSM", "Dont Ask", "B9", "Googol" ]
          , queries:
              [ "Yeehaw", "Yeehaw", "Googol", "B9", "Googol"
              , "NSM", "B9", "NSM", "Dont Ask", "Googol"
              ]
          }
      solveCase case1 `shouldEqual` 1

    it "solves sample case #2" do
      let
        case2 :: Case
        case2 = Case
          { engines: [ "Yeehaw", "NSM", "Dont Ask", "B9", "Googol" ]
          , queries:
              [ "Googol", "Dont Ask", "NSM", "NSM"
              , "Yeehaw", "Yeehaw", "Googol"
              ]
          }
      solveCase case2 `shouldEqual` 0

    it "solves a case where a switch is required" do
      let
        case3 :: Case
        case3 = Case { engines: [ "A", "B" ], queries: [ "A", "B" ] }
      solveCase case3 `shouldEqual` 1

    it "solves a case where no switch is required (has 3rd option)" do
      let
        case4 :: Case
        case4 = Case { engines: [ "A", "B", "C" ], queries: [ "A", "B" ] }
      solveCase case4 `shouldEqual` 0

  -- Tests for formatResults
  describe "SavingTheUniverse.formatResults" do

    it "formats a single result" do
      -- Based on your format-case-result test
      formatResults [ 123 ] `shouldEqual` "Case #1: 123"

    it "formats multiple results" do
      -- Based on your solve-from-file integration test
      let
        results = [ 1, 0 ]
        expected = "Case #1: 1\nCase #2: 0"
      formatResults results `shouldEqual` expected

    it "formats zero results" do
      formatResults [] `shouldEqual` ""
