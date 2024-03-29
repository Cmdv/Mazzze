module Types where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Graphics.Gloss.Interface.IO.Interact
import System.Random (StdGen)


type Location = (Int, Int)

type Maze = Map.Map Location CellBoundaries

data GameResult = GameInProgress | GameWon
 deriving (Eq, Show)

data World = World
  { playerLocation       :: Location
  , startLocation        :: Location
  , endLocation          :: Location
  , worldBoundaries      :: Maze
  , gameResult           :: GameResult
  , worldRandomGenerator :: StdGen
  }

data BoundaryType = WorldBoundary | Wall | AdjacentCell Location

data CellBoundaries = CellBoundaries
  { upBoundary    :: BoundaryType
  , rightBoundary :: BoundaryType
  , downBoundary  :: BoundaryType
  , leftBoundary  :: BoundaryType
  }

data CellCoordinates = CellCoordinates
  { cellCenter      :: Point
  , cellTopLeft     :: Point
  , cellTopRight    :: Point
  , cellBottomLeft  :: Point
  , cellBottomRight :: Point
  }

data SearchState = SearchState
  { randomGen         :: StdGen
  , locationStack     :: [Location]
  , currentBoundaries :: Map.Map Location CellBoundaries
  , visitedCells      :: Set.Set Location
  }
