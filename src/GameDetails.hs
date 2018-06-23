module GameDetails where

import Interpretor (GameDetails(..),
                    BuildingType(..),
                    BuildingStats(..),
                    TowerStats(..))

towerStats :: BuildingType -> GameDetails -> TowerStats
towerStats ATTACK  = attackTowerStats  . buildingsStats
towerStats DEFENSE = defenseTowerStats . buildingsStats
towerStats ENERGY  = energyTowerStats  . buildingsStats
towerStats TESLA   = teslaTowerStats   . buildingsStats
