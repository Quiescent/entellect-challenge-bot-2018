module Bot
  where

import Interpretor (GameState(..),
                    Command(..),
                    GameDetails(..),
                    CellStateContainer(..),
                    BuildingType(..))
import Player
import Cell
import Towers
import Data.List as L
import System.Random
import Control.Monad
import Data.Vector as V

-- Predicate combination operator
(&&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(&&&) f g = \ input -> f input && g input

enemyHasAttacking :: GameState -> Int -> Bool
enemyHasAttacking state =
  V.any cellContainsEnemyAttacker . ((gameMap state) V.!)
  where
    cellContainsEnemyAttacker =
      cellBelongsToOponent &&& (cellContainsBuildingType ATTACK)

iDontHaveDefense :: GameState -> Int -> Bool
iDontHaveDefense state =
  not . V.any cellContainDefenseFromMe . ((gameMap state) V.!)
  where
    cellContainDefenseFromMe =
      cellBelongsToMe &&& (cellContainsBuildingType DEFENSE)

thereIsAnEmptyCellInRow :: GameState -> Int -> Bool
thereIsAnEmptyCellInRow (GameState {gameMap = gameMap'})=
  V.any cellIsEmpty . (gameMap' V.!)

indexOfFirstEmpty :: GameState -> Int -> Maybe Int
indexOfFirstEmpty (GameState {gameMap = gameMap'}) =
  fmap yPos . V.find (cellIsEmpty &&& cellBelongsToMe) . (gameMap' V.!)

defendAttack :: GameState -> Maybe (Int, Int, BuildingType)
defendAttack state@(GameState _ _ (GameDetails _ _ height _)) = do
  x <- L.find rowUnderAttack [0..height - 1]
  y <- indexOfFirstEmpty state x
  return (x, y, DEFENSE)
  where
    rowUnderAttack = (enemyHasAttacking state) &&&
                     (iDontHaveDefense state) &&&
                     (thereIsAnEmptyCellInRow state)

hasEnoughEnergyForMostExpensiveBuilding :: GameState -> Bool
hasEnoughEnergyForMostExpensiveBuilding state =
  (ourEnergy state) >= maxPrice
  where
    maxPrice = L.maximum $ L.map fst $ towerPrices $ gameDetails state

myEmptyCells :: V.Vector (V.Vector CellStateContainer) -> [CellStateContainer]
myEmptyCells =
  L.concat . L.map (L.filter isMineAndIsEmpty . V.toList) . V.toList
  where
      isMineAndIsEmpty = cellIsEmpty &&& cellBelongsToMe

randomEmptyCell :: RandomGen g => g -> GameState -> ((Int, Int), g)
randomEmptyCell gen (GameState {gameMap = mapGrid}) =
  let emptyCells                = myEmptyCells mapGrid
      (randomInt, newGenerator) = next gen
      emptyCell                 = emptyCells !! mod randomInt (L.length emptyCells)
  in ((xPos emptyCell, yPos emptyCell), newGenerator)

randomBuilding :: RandomGen g => g -> (BuildingType, g)
randomBuilding gen =
  let (randomInt, gen') = next gen
      buildingIndex     = mod randomInt 3
  in (case buildingIndex of
        0 -> DEFENSE
        1 -> ATTACK
        _ -> ENERGY,
      gen')

buildRandomly :: RandomGen g => g -> GameState -> Maybe (Int, Int, BuildingType)
buildRandomly gen state =
  if not $ hasEnoughEnergyForMostExpensiveBuilding state
  then Nothing
  else let ((x, y),    gen') = randomEmptyCell gen  state
           (building', _)    = randomBuilding gen'
       in Just (x, y, building')

decide :: RandomGen g => g -> GameState -> Command
decide gen state =
  case msum [defendAttack state, buildRandomly gen state] of
    Just (x, y, building') -> Command x y building'
    Nothing                -> NothingCommand
