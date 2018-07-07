module BuildingsUnderConstruction (addBuilding,
                                   createBuildingUnderConstruction,
                                   tickConstruction,
                                   placeBuilding,
                                   buildingConstructionSites,
                                   containsSite,
                                   foldrConstruction)
  where

import Interpretor (BuildingUnderConstruction,
                    ConstructionQueue,
                    Building(..),
                    TowerMap)
import GameMap

import qualified Data.PQueue.Min as PQ
import qualified Data.Set        as S

createBuildingUnderConstruction :: Int -> Int -> Int -> Building -> BuildingUnderConstruction
createBuildingUnderConstruction timeLeft x' y' building' = (timeLeft, (x', y'), building')

-- TODO consider BitSet
type ConstructionSite = (Int, Int)
type ConstructionSites = S.Set (Int, Int)

buildingConstructionSites :: ConstructionQueue -> ConstructionSites
buildingConstructionSites = S.fromList . map (\ (_, x, _) -> x) . PQ.toList

containsSite :: ConstructionSite -> ConstructionSites -> Bool
containsSite = S.member

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

foldrConstruction :: (BuildingUnderConstruction -> b -> b) -> b -> ConstructionQueue -> b
foldrConstruction = PQ.foldrAsc
