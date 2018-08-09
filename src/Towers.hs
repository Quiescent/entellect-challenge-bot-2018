module Towers (towerPrices, towerCost)
  where

import Interpretor (Building,
                    BuildingType(..))
import Magic
import Buildings


attackTowerCostWithType :: (Int, BuildingType)
attackTowerCostWithType = (attackTowerCost, ATTACK)

defenseTowerCostWithType :: (Int, BuildingType)
defenseTowerCostWithType = (defenseTowerCost, DEFENSE)

energyTowerCostWithType :: (Int, BuildingType)
energyTowerCostWithType = (energyTowerCost, ENERGY)

teslaTowerCostWithType :: (Int, BuildingType)
teslaTowerCostWithType = (teslaTowerCost, TESLA)

towerCost :: Building -> Int
towerCost building' =
  inner building'
  where
    inner EnergyTower = energyTowerCost
    inner Attack3     = attackTowerCost
    inner Attack2     = attackTowerCost
    inner Attack1     = attackTowerCost
    inner Attack0     = attackTowerCost
    inner Defense4    = defenseTowerCost
    inner Defense3    = defenseTowerCost
    inner Defense2    = defenseTowerCost
    inner Defense1    = defenseTowerCost
    inner Tesla10     = teslaTowerCost
    inner  Tesla9     = teslaTowerCost
    inner  Tesla8     = teslaTowerCost
    inner  Tesla7     = teslaTowerCost
    inner  Tesla6     = teslaTowerCost
    inner  Tesla5     = teslaTowerCost
    inner  Tesla4     = teslaTowerCost
    inner  Tesla3     = teslaTowerCost
    inner  Tesla2     = teslaTowerCost
    inner  Tesla1     = teslaTowerCost
    inner  Tesla0     = teslaTowerCost

towerPrices :: [(Int, BuildingType)]
towerPrices =
  [attackTowerCostWithType,
   defenseTowerCostWithType,
   energyTowerCostWithType,
   teslaTowerCostWithType]
