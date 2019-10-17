module MazeParser where

import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (fromJust, catMaybes)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Void (Void)
import System.Random (StdGen, randomR)
import qualified Text.Megaparsec as M
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

    initialBounds :: Map.Map Location CellBoundaries
    initialBounds = case M.runParser (mazeParser (numRows, numColumns)) "" fullString of
      Right bounds -> bounds
      _ -> error "Couldn't parse maze for some reason!"

-- The game won't run until we implement this in part 5
mazeParser :: (Int, Int) -> MParser Maze
mazeParser (numRows, numColumns) = undefined

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
findCandidates currentLocation@(x, y) bounds visited =
  let currentLocBounds = fromJust $
        Map.lookup currentLocation bounds

      upLoc = (x , y + 1)
      maybeUpCell =
        case (upBoundary currentLocBounds, Set.member upLoc visited) of
          (Wall, False) -> Just
            ( upLoc
            , (fromJust $ Map.lookup upLoc bounds) { downBoundary = AdjacentCell currentLocation }
            , currentLocation
            , currentLocBounds { upBoundary = AdjacentCell upLoc }
            )
          _ -> Nothing

      rightLoc = (x + 1, y)
      maybeRightCell =
        case (rightBoundary currentLocBounds, Set.member rightLoc visited) of
          (Wall, False) -> Just
            ( rightLoc
            , (fromJust $ Map.lookup rightLoc bounds) { rightBoundary = AdjacentCell currentLocation }
            , currentLocation
            , currentLocBounds { rightBoundary = AdjacentCell rightLoc }
            )
          _ -> Nothing

      downLoc = (x , y - 1)
      maybeDownCell =
        case (downBoundary currentLocBounds, Set.member downLoc visited) of
          (Wall, False) -> Just
            ( downLoc
            , (fromJust $ Map.lookup downLoc bounds) { downBoundary = AdjacentCell currentLocation }
            , currentLocation
            , currentLocBounds { downBoundary = AdjacentCell downLoc }
            )
          _ -> Nothing

      leftLoc = (x , y - 1)
      maybeLeftCell =
        case (leftBoundary currentLocBounds, Set.member leftLoc visited) of
          (Wall, False) -> Just
            ( leftLoc
            , (fromJust $ Map.lookup leftLoc bounds) { leftBoundary = AdjacentCell currentLocation }
            , currentLocation
            , currentLocBounds { leftBoundary = AdjacentCell leftLoc }
            )
          _ -> Nothing

      in catMaybes [maybeUpCell, maybeRightCell, maybeDownCell, maybeLeftCell]
