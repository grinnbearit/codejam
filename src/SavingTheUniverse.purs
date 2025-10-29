module SavingTheUniverse where

import Prelude

import Data.Array (drop, fromFoldable, head, mapWithIndex, tail, take, (!!))
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.List.Types (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String.Common (split, joinWith)
import Effect (Effect)
import Effect.Console (error, log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Process (argv)

-- | Use simple String aliases for our domain
type EngineName = String
type Query = String

-- | A record type to hold the data for a single test case.
newtype Case = Case
  { engines :: Array EngineName
  , queries :: Array Query
  }

derive instance newtypeCase :: Newtype Case _
derive instance eqCase :: Eq Case

derive instance genericCase :: Generic Case _
instance showCase :: Show Case where
  show = genericShow

-- | A record type for the state we carry during the fold.
type SolverState =
  { switches :: Int
  , segmentEngines :: Set EngineName
  }

-- | Parses the entire input string into a list of cases.
parseInput :: String -> Maybe (List Case)
parseInput input =
  let
    lines = split (Pattern "\n") input

    go :: Array String -> Int -> Maybe (List Case)
    go _ 0 = pure Nil
    go ls n = do
      { caseData, remaining } <- parseSingleCase ls
      remainingCases <- go remaining (n - 1)
      pure (caseData : remainingCases)

  in
    do
      nStr <- head lines
      numCases <- fromString nStr
      lineList <- tail lines
      go lineList numCases

-- | Parses one case from a list of lines.
parseSingleCase :: Array String -> Maybe { caseData :: Case, remaining :: Array String }
parseSingleCase lines = do
  sStr <- head lines
  linesAfterS <- tail lines
  s <- fromString sStr

  let engines = take s linesAfterS
  let linesAfterEngines = drop s linesAfterS

  qStr <- head linesAfterEngines
  linesAfterQ <- tail linesAfterEngines
  q <- fromString qStr

  let queries = take q linesAfterQ
  let linesAfterQueries = drop q linesAfterQ

  pure
    { caseData: Case { engines: engines, queries: queries }
    , remaining: linesAfterQueries
    }

-- | This is the core logic, identical to your `solve-case`.
solveCase :: Case -> Int
solveCase (Case { engines, queries }) =
  let
    s = Array.length engines

    stepFn :: SolverState -> Query -> SolverState
    stepFn state query =
      if Set.member query state.segmentEngines then
        state
      else
        let
          segmentSize = Set.size state.segmentEngines
        in
          if segmentSize == (s - 1) then
            { switches: state.switches + 1
            , segmentEngines: Set.singleton query
            }
          else
            { switches: state.switches
            , segmentEngines: Set.insert query state.segmentEngines
            }

    initialState :: SolverState
    initialState = { switches: 0, segmentEngines: Set.empty }

    finalState = foldl stepFn initialState queries
  in
    finalState.switches

-- | Formats a list of integer results into the "Case #X: Y" output string.
formatResults :: Array Int -> String
formatResults =
  let
    formatLine :: Int -> Int -> String
    formatLine idx result =
      "Case #" <> show (idx + 1) <> ": " <> show result
  in
    (joinWith "\n") <<< (mapWithIndex formatLine)


parseFile :: String -> Effect (Maybe (List Case))
parseFile path = do
  content <- readTextFile UTF8 path
  pure (parseInput content)

-- | The "point-free" definition of our solution pipeline.
solve :: List Case -> String
solve = formatResults <<< map solveCase <<< fromFoldable


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
          error "Check file format, N, S, and Q values."

        Just cases -> do
          log $ solve cases
