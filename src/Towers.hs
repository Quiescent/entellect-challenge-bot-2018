module Towers (towerPrices, towerCost)
  where

import Interpretor (Building(..),
                    BuildingType(..))
import Magic
import Buildings
import VectorIndex

import qualified Data.Vector.Unboxed as UV

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
  results `uVectorIndex` building'
  where
    results = UV.fromList $ map inner [energyTower..tesla0]
    inner building''
      | building'' == energyTower = energyTowerCost
      | building'' == attack3     = attackTowerCost
      | building'' == attack2     = attackTowerCost
      | building'' == attack1     = attackTowerCost
      | building'' == attack0     = attackTowerCost
      | building'' == defense4    = defenseTowerCost
      | building'' == defense3    = defenseTowerCost
      | building'' == defense2    = defenseTowerCost
      | building'' == defense1    = defenseTowerCost
      | building'' == tesla10     = teslaTowerCost
      | building'' ==  tesla9     = teslaTowerCost
      | building'' ==  tesla8     = teslaTowerCost
      | building'' ==  tesla7     = teslaTowerCost
      | building'' ==  tesla6     = teslaTowerCost
      | building'' ==  tesla5     = teslaTowerCost
      | building'' ==  tesla4     = teslaTowerCost
      | building'' ==  tesla3     = teslaTowerCost
      | building'' ==  tesla2     = teslaTowerCost
      | building'' ==  tesla1     = teslaTowerCost
      | building'' ==  tesla0     = teslaTowerCost

towerPrices :: [(Int, BuildingType)]
towerPrices =
  [attackTowerCostWithType,
   defenseTowerCostWithType,
   energyTowerCostWithType,
   teslaTowerCostWithType]
