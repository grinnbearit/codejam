module TrainTimetable where

import Prelude

import Control.Alternative (guard)
import Data.Array (drop, head, mapWithIndex, tail, take, (!!))
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String.Common (split, joinWith)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (error, log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Process (argv)

type Time = Int -- Minutes since midnight

newtype Trip = Trip
  { dep :: Time
  , arr :: Time
  }

derive instance newtypeTrip :: Newtype Trip _
derive instance genericTrip :: Generic Trip _
derive instance eqTrip :: Eq Trip
instance showTrip :: Show Trip where
  show = genericShow


newtype Case = Case
  { t :: Int
  , tripsAB :: Array Trip
  , tripsBA :: Array Trip
  }

derive instance newtypeCase :: Newtype Case _
derive instance genericCase :: Generic Case _
derive instance eqCase :: Eq Case
instance showCase :: Show Case where
  show = genericShow


newtype Result = Result
  { trainsA :: Int
  , trainsB :: Int
  }

derive instance newtypeResult :: Newtype Result _
derive instance genericResult :: Generic Result _
derive instance eqResult :: Eq Result
instance showResult :: Show Result where
  show = genericShow

-- | This is our idiomatic `time-to-int`
timeToInt :: String -> Maybe Time
timeToInt s =
  case split (Pattern ":") s of
    [hStr, mStr] -> do
      h <- fromString hStr
      m <- fromString mStr
      guard $ h >= 0 && h <= 23
      guard $ m >= 0 && m <= 59
      pure $ h * 60 + m
    _ ->
      Nothing

-- | Helper to parse a single trip line
parseTrip :: String -> Maybe Trip
parseTrip s =
  case split (Pattern " ") s of
    [depStr, arrStr] -> do
      depTime <- timeToInt depStr
      arrTime <- timeToInt arrStr
      pure $ Trip { dep: depTime, arr: arrTime }
    _ ->
      Nothing

-- | This is the parser for the Train Timetable problem
parseSingleCase :: Array String -> Maybe { caseData :: Case, remaining :: Array String }
parseSingleCase lines = do
  tStr <- head lines
  t <- fromString tStr

  naNbLine <- lines !! 1

  case split (Pattern " ") naNbLine of
    [naStr, nbStr] -> do
      na <- fromString naStr
      nb <- fromString nbStr

      let tripLines = drop 2 lines
      let linesAB = take na tripLines
      let linesBA = take nb (drop na tripLines)
      let remaining = drop (na + nb) tripLines

      tripsAB <- traverse parseTrip linesAB
      tripsBA <- traverse parseTrip linesBA

      pure
        { caseData: Case { t: t, tripsAB: tripsAB, tripsBA: tripsBA }
        , remaining: remaining
        }

    _ ->
      Nothing

-- | The core greedy algorithm, translated from Clojure
calculateTrains :: Int -> Array Time -> Array Time -> Int
calculateTrains t departures arrivals =
  let
    -- Convert to List and sort
    sortedDs = List.sort (List.fromFoldable departures)
    sortedRs = List.sort (map (\arr -> arr + t) (List.fromFoldable arrivals))

    -- Recursive helper, identical to the Clojure `loop`
    go :: List Time -> List Time -> Int -> Int
    go Nil _ acc = acc -- Base case: No more departures
    go (_ : ds) Nil acc = go ds Nil (acc + 1) -- No ready trains, need new one
    go (d : ds) (r : rs) acc
      | r <= d = go ds rs acc -- Reuse train: consume d and r
      | otherwise = go ds (r : rs) (acc + 1) -- Need new train: consume d, keep r
  in
    go sortedDs sortedRs 0

-- | The main solving logic for this problem
solveCase :: Case -> Result
solveCase (Case c) =
  let
    -- Extract all departure and arrival times
    departuresA = map (unwrap >>> _.dep) c.tripsAB
    arrivalsB = map (unwrap >>> _.arr) c.tripsAB

    departuresB = map (unwrap >>> _.dep) c.tripsBA
    arrivalsA = map (unwrap >>> _.arr) c.tripsBA

    -- Calculate trains needed at each station
    trainsA = calculateTrains c.t departuresA arrivalsA
    trainsB = calculateTrains c.t departuresB arrivalsB
  in
    Result { trainsA: trainsA, trainsB: trainsB }


formatResults :: Array Result -> String
formatResults =
  let
    formatResult :: Int -> Result -> String
    formatResult idx (Result r) =
      "Case #" <> show (idx + 1) <> ": " <> show r.trainsA <> " " <> show r.trainsB
  in
    joinWith "\n" <<< mapWithIndex formatResult

-- | solve :: List Case -> String
solve :: List Case -> String
solve = formatResults <<< map solveCase <<< Array.fromFoldable


parseFile :: String -> Effect (Maybe (List Case))
parseFile path = do
  content <- readTextFile UTF8 path
  pure $ parseInput content


parseInput :: String -> Maybe (List Case)
parseInput input =
  let
    lines = split (Pattern "\n") input
    go :: Array String -> Int -> Maybe (List Case)
    go _ 0 = pure Nil
    go ls n = do
      { caseData, remaining } <- parseSingleCase ls
      remainingCases <- go remaining (n - 1)
      pure $ caseData : remainingCases
  in
    do
      nStr <- head lines
      numCases <- fromString nStr
      lineList <- tail lines
      go lineList numCases


main :: Effect Unit
main = do
  args <- argv
  case args !! 2 of
    Nothing -> do
      error "Error: No input file specified."
      error "Usage: spago run -b <path-to-input-file>"
    Just path -> do
      maybeCases <- parseFile path
      case maybeCases of
        Nothing -> do
          error "Error: Failed to parse input file."
          error "Check file format, N, S, Q, T, NA, or NB values."

        Just cases -> do
          log $ solve cases
