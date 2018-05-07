module Bot
  where

import Interpretor (GameState(..),
                    Command,
                    GameDetails(..),
                    Building(..),
                    CellStateContainer(..),
                    PlayerType(..),
                    BuildingType(..),
                    BuildingPriceIndex(..),
                    Player(..))
import Data.List

bothPredicates :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
bothPredicates f g = \ input -> f input && g input

cellBelongsTo :: PlayerType -> CellStateContainer -> Bool
cellBelongsTo typeOfPlayer =
  (==typeOfPlayer) . cellOwner

cellContainsBuildingType :: BuildingType -> CellStateContainer -> Bool
cellContainsBuildingType typeOfBuilding =
  any (==typeOfBuilding) . map buildingType . buildings

enemyHasAttacking :: GameState -> Int -> Bool
enemyHasAttacking state row =
  any cellContainsEnemyAttacker ((gameMap state) !! row)
  where
    cellContainsEnemyAttacker =
      bothPredicates (cellBelongsTo B) (cellContainsBuildingType Attack)

iDontHaveDefense :: GameState -> Int -> Bool
iDontHaveDefense state row =
  any cellDoesntContainDefenseFromMe ((gameMap state) !! row)
  where
    cellDoesntContainDefenseFromMe =
      bothPredicates (cellBelongsTo A) (cellContainsBuildingType Defense)

underAttack :: GameState -> Maybe Int
underAttack state@(GameState _ _ (GameDetails _ width _ _)) =
  find rowUnderAttack [0..width - 1]
  where
    rowUnderAttack = bothPredicates (enemyHasAttacking state) (iDontHaveDefense state)

hasEnoughEnergyForMostExpensiveBuilding :: GameState -> Bool
hasEnoughEnergyForMostExpensiveBuilding state@(GameState _ _ (GameDetails { buildingPrices = prices })) =
  ourEnergy >= maxPrice
  where
    ourEnergy = energy ourPlayer
    ourPlayer = (head . filter ((==A) . playerType) . players) state
    maxPrice = maximum towerPrices
    towerPrices = map ($ prices) [attackTowerCost, defenseTowerCost, energyTowerCost]

doNothingCommand :: Command
doNothingCommand = ""

defendRow :: GameState -> Command
defendRow _ = doNothingCommand

buildRandom :: GameState -> Command
buildRandom _ = doNothingCommand
