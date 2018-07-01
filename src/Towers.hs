module Towers (towerPrices)
  where

import Interpretor (GameDetails(..),
                    BuildingPriceIndex(..),
                    BuildingType(..))

attackTowerCostWithType :: BuildingPriceIndex -> (Int, BuildingType)
attackTowerCostWithType index = (attackTowerCost index, ATTACK)

defenseTowerCostWithType :: BuildingPriceIndex -> (Int, BuildingType)
defenseTowerCostWithType index = (defenseTowerCost index, DEFENSE)

energyTowerCostWithType :: BuildingPriceIndex -> (Int, BuildingType)
energyTowerCostWithType index = (energyTowerCost index, ENERGY)

teslaTowerCostWithType :: BuildingPriceIndex -> (Int, BuildingType)
teslaTowerCostWithType index = (teslaTowerCost index, TESLA)

towerPrices :: GameDetails -> [(Int, BuildingType)]
towerPrices (GameDetails { buildingPrices = prices }) =
  fmap ($ prices) [attackTowerCostWithType,
                   defenseTowerCostWithType,
                   energyTowerCostWithType,
                   teslaTowerCostWithType]
