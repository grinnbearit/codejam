module Test.TrainTimetable where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import TrainTimetable (Case(..), Result(..), Trip(..), calculateTrains, formatResults, parseInput, solveCase, timeToInt)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- Our test suite's main entry point
main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec

-- Our main spec
spec :: Spec Unit
spec = do

  -- Tests for new helper functions
  describe "TrainTimetable.timeToInt" do
    it "converts 'HH:MM' to minutes" do
      timeToInt "00:00" `shouldEqual` (Just 0)
      timeToInt "01:00" `shouldEqual` (Just 60)
      timeToInt "09:30" `shouldEqual` (Just 570)
      timeToInt "23:59" `shouldEqual` (Just 1439)
    it "returns Nothing for invalid time" do
      timeToInt "24:00" `shouldEqual` Nothing
      timeToInt "09:60" `shouldEqual` Nothing
      timeToInt "foo" `shouldEqual` Nothing

  -- Tests for parsing
  describe "TrainTimetable.parseInput" do
    it "parses the full sample input" do
      let input = "2\n5\n3 2\n09:00 12:00\n10:00 13:00\n11:00 12:30\n12:02 15:00\n09:00 10:30\n2\n2 0\n09:00 09:01\n12:00 12:02"
      let
        expected = Just
          [ Case
              { t: 5
              , tripsAB:
                  [ Trip { dep: 540, arr: 720 }
                  , Trip { dep: 600, arr: 780 }
                  , Trip { dep: 660, arr: 750 }
                  ]
              , tripsBA:
                  [ Trip { dep: 722, arr: 900 }
                  , Trip { dep: 540, arr: 630 }
                  ]
              }
          , Case
              { t: 2
              , tripsAB:
                  [ Trip { dep: 540, arr: 541 }
                  , Trip { dep: 720, arr: 722 }
                  ]
              , tripsBA: []
              }
          ]
      let actual = Array.fromFoldable <$> parseInput input
      actual `shouldEqual` expected

  -- Tests for the core logic
  describe "TrainTimetable.calculateTrains" do
    it "needs 1 train if one departure and no arrivals" do
      calculateTrains 5 [ 100 ] [] `shouldEqual` 1
    it "needs 0 trains if no departures" do
      calculateTrains 5 [] [ 100 ] `shouldEqual` 0
    it "needs 0 trains if arrival is ready in time" do
      calculateTrains 5 [ 100 ] [ 90 ] `shouldEqual` 0 -- Ready at 95
    it "needs 1 train if arrival is too late" do
      calculateTrains 15 [ 100 ] [ 90 ] `shouldEqual` 1 -- Ready at 105
    it "correctly reuses multiple trains" do
      calculateTrains 5 [ 100, 200 ] [ 90, 190 ] `shouldEqual` 0
    it "correctly handles shortage of trains" do
      calculateTrains 5 [ 100, 110 ] [ 90 ] `shouldEqual` 1
    it "correctly handles ready-time being too late" do
      calculateTrains 5 [ 100, 110 ] [ 105 ] `shouldEqual` 1

  -- Tests for the main case solver
  describe "TrainTimetable.solveCase" do
    it "solves sample case #1" do
      let
        case1 = Case
          { t: 5
          , tripsAB:
              [ Trip { dep: 540, arr: 720 }
              , Trip { dep: 600, arr: 780 }
              , Trip { dep: 660, arr: 750 }
              ]
          , tripsBA:
              [ Trip { dep: 722, arr: 900 }
              , Trip { dep: 540, arr: 630 }
              ]
          }
      solveCase case1 `shouldEqual` (Result { trainsA: 2, trainsB: 2 })

    it "solves sample case #2" do
      let
        case2 = Case
          { t: 2
          , tripsAB:
              [ Trip { dep: 540, arr: 541 }
              , Trip { dep: 720, arr: 722 }
              ]
          , tripsBA: []
          }
      solveCase case2 `shouldEqual` (Result { trainsA: 2, trainsB: 0 })

  -- Tests for formatting
  describe "TrainTimetable.formatResults" do
    it "formats multiple results" do
      let
        results = [ Result { trainsA: 2, trainsB: 2 }, Result { trainsA: 2, trainsB: 0 } ]
        expected = "Case #1: 2 2\nCase #2: 2 0"
      formatResults results `shouldEqual` expected
