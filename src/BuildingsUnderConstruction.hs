module BuildingsUnderConstruction (addBuilding,
                                   createBuildingUnderConstruction,
                                   tickConstruction,
                                   placeBuilding)
  where

import Interpretor (BuildingUnderConstruction,
                    ConstructionQueue,
                    Building(..),
                    TowerMap)
import GameMap

import qualified Data.PQueue.Min as PQ

createBuildingUnderConstruction :: Int -> Int -> Int -> Building -> BuildingUnderConstruction
createBuildingUnderConstruction timeLeft x' y' building' = (timeLeft, (x', y'), building')

addBuilding :: BuildingUnderConstruction -> ConstructionQueue -> ConstructionQueue
addBuilding = PQ.insert

tickConstruction :: ConstructionQueue -> ([BuildingUnderConstruction], ConstructionQueue)
tickConstruction = PQ.span constructed . PQ.map decrementConstructionTime

constructed :: BuildingUnderConstruction -> Bool
constructed (timeLeft, _, _) = timeLeft < 0

decrementConstructionTime :: BuildingUnderConstruction -> BuildingUnderConstruction
decrementConstructionTime (time, x, y) = (time - 1, x, y)

placeBuilding :: BuildingUnderConstruction -> TowerMap -> TowerMap
placeBuilding (_, coord, building') = addAt coord building'
