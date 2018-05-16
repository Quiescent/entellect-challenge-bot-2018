module Cell (cellBelongsToOponent,
             cellBelongsToMe,
             cellIsEmpty,
             cellContainsBuildingType,
             allCells,
             removeMissiles,
             addMissile)
  where

import Interpretor (CellContents(..),
                    BuildingType(..),
                    Building(..),
                    GameState(..),
                    GameDetails(..),
                    Missile(..),
                    SparseMap)
import Data.Map.Strict as M
import Data.Vector     as V

allCells :: GameState -> [(Int, Int)]
allCells (GameState {gameDetails = details}) =
  [(x, y) | x <- [0..(mapWidth details) - 1], y <- [0..(mapHeight details) - 1]]

cellIsEmpty :: SparseMap -> (Int, Int) -> Bool
cellIsEmpty = flip M.member

-- TODO: which is width and which is height? (for both of the next
-- two)
cellBelongsToMe :: GameState -> (Int, Int) -> Bool
cellBelongsToMe (GameState {gameDetails = details}) =
  (< (div (mapWidth details) 2)) . snd

cellBelongsToOponent :: GameState -> (Int, Int) -> Bool
cellBelongsToOponent state = not . (cellBelongsToMe state)

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

cellContainsBuildingType :: BuildingType -> CellContents -> Bool
cellContainsBuildingType typeOfBuilding =
  isJust . fmap (((==typeOfBuilding) . buildingType)) . buildingInCell

removeMissiles :: CellContents -> CellContents
removeMissiles (CellContents building _) = CellContents building V.empty

addMissile :: Missile -> CellContents -> CellContents
addMissile missile cellContents@(CellContents _ missiles') =
  cellContents { missilesInCell = V.cons missile missiles' }
