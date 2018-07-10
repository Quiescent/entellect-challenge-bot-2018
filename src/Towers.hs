module Towers (towerPrices, towerCost)
  where

import Interpretor (BuildingType(..))
import Magic

attackTowerCostWithType :: (Int, BuildingType)
attackTowerCostWithType = (attackTowerCost, ATTACK)

defenseTowerCostWithType :: (Int, BuildingType)
defenseTowerCostWithType = (defenseTowerCost, DEFENSE)

energyTowerCostWithType :: (Int, BuildingType)
energyTowerCostWithType = (energyTowerCost, ENERGY)

teslaTowerCostWithType :: (Int, BuildingType)
teslaTowerCostWithType = (teslaTowerCost, TESLA)

towerCost :: BuildingType -> Int
towerCost ATTACK = attackTowerCost
towerCost DEFENSE = defenseTowerCost
towerCost ENERGY = energyTowerCost
towerCost TESLA = teslaTowerCost

towerPrices :: [(Int, BuildingType)]
towerPrices =
  [attackTowerCostWithType,
   defenseTowerCostWithType,
   energyTowerCostWithType,
   teslaTowerCostWithType]
