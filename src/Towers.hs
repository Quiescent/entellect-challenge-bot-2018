module Towers (towerPrices)
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

towerPrices :: [(Int, BuildingType)]
towerPrices =
  [attackTowerCostWithType,
   defenseTowerCostWithType,
   energyTowerCostWithType,
   teslaTowerCostWithType]
