module Cell (cellBelongsToHim, cellBelongsToMe, cellIsEmpty, cellContainsBuildingType)
  where

import Interpretor (CellStateContainer(..),
                    PlayerType(..),
                    BuildingType(..),
                    Building(..))
import Data.Vector as V

cellBelongsTo :: PlayerType -> CellStateContainer -> Bool
cellBelongsTo typeOfPlayer =
  (==typeOfPlayer) . cellOwner

cellBelongsToMe :: CellStateContainer -> Bool
cellBelongsToMe = cellBelongsTo A

cellBelongsToHim :: CellStateContainer -> Bool
cellBelongsToHim = cellBelongsTo B

cellIsEmpty :: CellStateContainer -> Bool
cellIsEmpty = (V.empty ==) . buildings

cellContainsBuildingType :: BuildingType -> CellStateContainer -> Bool
cellContainsBuildingType typeOfBuilding =
  V.any ((==typeOfBuilding) . buildingType) . buildings
