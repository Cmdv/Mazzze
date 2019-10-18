module MazeParser where

import Control.Monad.State
import Data.Char
import Data.List (groupBy)
import qualified Data.Map as Map
import Data.Maybe (fromJust, catMaybes)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Void (Void)
import System.Random (StdGen, randomR)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import Types


type MParser = M.Parsec Void Text

generateRandomMaze :: StdGen -> (Int, Int) -> (Maze, StdGen)
generateRandomMaze gen (numRows, numColumns) =
  (currentBoundaries finalState, randomGen finalState)
  where
    (startX, g1) = randomR (0, numColumns - 1) gen
    (startY, g2) = randomR (0, numColumns - 1) g1
    finalState   = execState depthFirstSearch initialState

    initialState :: SearchState
    initialState = SearchState
      g2
      [(startX, startY)]
      initialBounds
      (Set.fromList [(startX, startY)])

    fullString :: Text
    fullString = pack . unlines $ replicate numRows (replicate numColumns 'F')

    initialBounds :: Maze
    initialBounds = case M.runParser (mazeParser (numRows, numColumns)) "" fullString of
      Left _ -> error "Maze can't be parsed!"
      Right success -> success

mazeParser :: (Int, Int) -> MParser Maze
mazeParser (numRows, numColumns) = do
  rows <- forM [0..(numRows - 1)] $
            \i -> do
              columns <-
                forM [0..(numColumns - 1)] $
                  \j -> do
                    c <- M.hexDigitChar
                    return (j, c)
              _ <- M.newline
              return $ map (\(col, char) -> ((col, i), char)) columns
  return $ Map.fromList (cellSpecToBound <$> concat rows)
  where
    cellSpecToBound :: (Location, Char) -> (Location, CellBoundaries)
    cellSpecToBound (loc@(x, y), c) = do
      let (topIsWall, rightIsWall, downIsWall, leftIsWall) = charToBoundsSet c
          topCell   = getBoundsCell topIsWall (y + 1 == numRows) (x, y + 1)
          rightCell = getBoundsCell rightIsWall (x + 1 == numColumns) (x + 1, y)
          downCell  = getBoundsCell downIsWall (y == 0) (x, y -1)
          leftCell  = getBoundsCell leftIsWall (x == 0) (x - 1, y)
      (loc, CellBoundaries topCell rightCell downCell leftCell)

getBoundsCell :: Bool -> Bool -> (Int, Int) -> BoundaryType
getBoundsCell isWall eqRows coords =
      if isWall
        then if eqRows
          then WorldBoundary
          else Wall
        else AdjacentCell coords

charToBoundsSet :: Char -> (Bool, Bool, Bool, Bool)
charToBoundsSet c =
  ( num > 7
  , num `mod` 8 > 3
  , num `mod` 4 > 1
  , num `mod` 2 == 1
  )
  where
    num = digitToInt c

cellToChar :: CellBoundaries -> Char
cellToChar bounds = do
  let top   = case upBoundary bounds of
                (AdjacentCell _) -> 0
                _ -> 8
      right = case upBoundary bounds of
                (AdjacentCell _) -> 0
                _ -> 4
      down  = case downBoundary bounds of
                (AdjacentCell _) -> 0
                _ -> 2
      left  = case leftBoundary bounds of
                (AdjacentCell _) -> 0
                _ -> 1
  toUpper $ intToDigit (top + right + down + left)

dumpMaze :: Maze -> Text
dumpMaze maze = pack $ (unlines . reverse) (rowToString <$> cellsByRow)
  where
    rowToString :: [(Location, CellBoundaries)] -> String
    rowToString = map (cellToChar . snd)

    transposedMap :: Maze
    transposedMap = Map.mapKeys (\(x, y) -> (y, x)) maze

    cellsByRow :: [[(Location, CellBoundaries)]]
    cellsByRow = groupBy (\((r1, _), _) ((r2, _), _) -> r1 == r2) (Map.toList transposedMap)


depthFirstSearch :: State SearchState ()
depthFirstSearch = do
  (SearchState gen locs bounds visited) <- get
  case locs of
    [] -> return ()
    (currentLoc : rest) -> do
      let candidateLocs = findCandidates currentLoc bounds visited
      if null candidateLocs
        then put (SearchState gen rest bounds visited) >> depthFirstSearch
        else chooseCandidate candidateLocs >> depthFirstSearch

chooseCandidate :: [(Location, CellBoundaries, Location, CellBoundaries)] -> State SearchState ()
chooseCandidate candidates = do
  (SearchState gen currentLocs boundsMap visited) <- get
  let (randomIndex, newGen) = randomR (0, length candidates - 1) gen
      (chosenLocation, newChosenBounds, prevLocation, newPrevBounds) = candidates !! randomIndex
      newBounds = Map.insert prevLocation newPrevBounds
                    (Map.insert chosenLocation newChosenBounds boundsMap)
      newVisited = Set.insert chosenLocation visited
      newSearchStack = chosenLocation : currentLocs
  put (SearchState newGen newSearchStack newBounds newVisited)

findCandidates ::
  Location ->
  -- | Current location
  Map.Map Location CellBoundaries ->
  -- | Current Maze state
  Set.Set Location ->
  -- | Visited Cells
  [(Location, CellBoundaries, Location, CellBoundaries)]
findCandidates currentLocation@(x, y) bounds visited = do

  let currentLocBounds = fromJust $ Map.lookup currentLocation bounds
      maybeUpCell = getMaybeCell upBoundary (x , y + 1) currentLocBounds bounds currentLocation visited
      maybeRightCell = getMaybeCell rightBoundary (x + 1, y) currentLocBounds bounds currentLocation visited
      maybeDownCell = getMaybeCell downBoundary (x , y - 1) currentLocBounds bounds currentLocation visited
      maybeLeftCell = getMaybeCell leftBoundary (x , y - 1) currentLocBounds bounds currentLocation visited

  catMaybes [maybeUpCell, maybeRightCell, maybeDownCell, maybeLeftCell]


getMaybeCell :: (CellBoundaries -> BoundaryType)
              -> (Int, Int)
              -> CellBoundaries
              -> Map.Map Location CellBoundaries
              -> Location
              -> Set.Set Location
              -> Maybe ((Int, Int), CellBoundaries, Location, CellBoundaries)
getMaybeCell boundary loc currentLocBounds bounds currentLocation visited = do
  let locBounds = fromJust $ Map.lookup loc bounds
      bound = case boundary of
            upBoundary    -> ( locBounds { upBoundary = AdjacentCell currentLocation }
                              , currentLocBounds { upBoundary = AdjacentCell loc })
            rightBoundary -> ( locBounds { rightBoundary = AdjacentCell currentLocation }
                              , currentLocBounds { rightBoundary = AdjacentCell loc })
            downBoundary  -> ( locBounds { downBoundary = AdjacentCell currentLocation }
                              , currentLocBounds { downBoundary = AdjacentCell loc })
            leftBoundary  -> ( locBounds { leftBoundary = AdjacentCell currentLocation }
                              , currentLocBounds { leftBoundary = AdjacentCell loc })
  case (boundary currentLocBounds, Set.member loc visited) of
      (Wall, False) -> Just
        ( loc
        , fst bound
        , currentLocation
        , snd bound
        )
      _ -> Nothing
