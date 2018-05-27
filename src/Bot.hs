module Bot
  where

import Interpretor (GameState(..),
                    Command(..),
                    BuildingType(..))
import SearchSpace
import Player
import Towers
import Data.List as L
import System.Random
import Control.Monad

import Debug.Trace

hasEnoughEnergyForMostExpensiveBuilding :: GameState -> Bool
hasEnoughEnergyForMostExpensiveBuilding state =
  (myEnergy state) >= maxPrice
  where
    maxPrice = L.maximum $ L.map fst $ towerPrices $ gameDetails state

randomEmptyCell :: RandomGen g => g -> GameState -> ((Int, Int), g)
randomEmptyCell gen state@(GameState {gameMap = mapGrid}) =
  let emptyCells                = L.filter (cellIsEmpty mapGrid) $ allCells state
      (randomInt, newGenerator) = next gen
      emptyCell                 = emptyCells !! mod randomInt (L.length emptyCells)
  in (emptyCell, newGenerator)

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
  case msum [buildRandomly gen state] of
    Just (x, y, building') -> trace ("Advanced: " ++ show (advanceState state)) (Command x y building')
    Nothing                -> NothingCommand
