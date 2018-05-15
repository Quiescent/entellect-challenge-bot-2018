module Cell (cellBelongsToOponent, cellBelongsToMe, cellIsEmpty, cellContainsBuildingType, allCells)
  where

import Interpretor (CellContents(..),
                    BuildingType(..),
                    Building(..),
                    GameState(..),
                    GameDetails(..),
                    SparseMap)
import Data.Map.Strict as M

allCells :: GameState -> [(Int, Int)]
allCells (GameState {gameDetails = details}) =
  [(x, y) | x <- [0..(mapWidth details) - 1], y <- [0..(mapHeight details) - 1]]

cellIsEmpty :: SparseMap -> (Int, Int) -> Bool
cellIsEmpty = flip M.member

cellBelongsToMe :: GameState -> (Int, Int) -> Bool
cellBelongsToMe _ _ = True

cellBelongsToOponent :: GameState -> (Int, Int) -> Bool
cellBelongsToOponent _ _ = True

cellContainsBuildingType :: BuildingType -> CellContents -> Bool
cellContainsBuildingType typeOfBuilding =
  ((==typeOfBuilding) . buildingType) . buildingInCell
