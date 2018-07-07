{-# LANGUAGE BangPatterns #-}

module Objective (myBoardScore, Move(..))
  where

import Interpretor (GameState(..),
                    Command(..),
                    Building(..),
                    BuildingType(..),
                    Player(..),
                    Row,
                    TowerMap)
import GameMap
import Row
import Player
import Magic
import BuildingsUnderConstruction

data Move = Move { myMove       :: Command,
                   oponentsMove :: Command }
          deriving (Show)

myBoardScore :: (GameState, a) -> (Float, (GameState, a))
myBoardScore withMove@(state, _) =
  (hitsSubtractTakenAfterTime withPlacedBuildings +
   hitsDealtToOponent state -
   hitsTakenByMe state -
   turnsToNextTowerByTurnByMultiplier meWithPlacedBuildings,
    withMove)
  where
    withPlacedBuildings         = (GameState meWithPlacedBuildings oponentWithPlacedBuildings)
    meWithPlacedBuildings       = (me      state) { towerMap = myWithPlacedBuildings }
    oponentWithPlacedBuildings  = (oponent state) { towerMap = oponentsWithPlacedBuildings }
    myConstructionQueue         = constructionQueue $ me state
    myTowerMap                  = towerMap $ me      state
    myWithPlacedBuildings       = foldrConstruction placeBuilding myTowerMap myConstructionQueue
    oponentsConstructionQueue   = constructionQueue $ oponent state
    oponentsTowerMap            = towerMap $ oponent state
    oponentsWithPlacedBuildings = foldrConstruction placeBuilding oponentsTowerMap oponentsConstructionQueue

hitsMultiplier :: Float
hitsMultiplier = 10

hitsDealtToOponent :: GameState -> Float
hitsDealtToOponent = (*hitsMultiplier) . fromIntegral . hitsTaken . oponentsPlayer

hitsTakenByMe :: GameState -> Float
hitsTakenByMe = (*hitsMultiplier) . fromIntegral . hitsTaken . myPlayer

hitsSubtractTakenAfterTime :: GameState -> Float
hitsSubtractTakenAfterTime (GameState me' oponent') =
  sum $ zipWith matchDefenseToAttack (attackAndDefensePerRow me') (attackAndDefensePerRow oponent')

matchDefenseToAttack :: (Float, Float) -> (Float, Float) -> Float
matchDefenseToAttack (myAttackPerTurn, myDefense) (oponentsAttackPerTurn, oponentsDefense) =
  turnsUntilHeBreaksThrough - turnsUntilIBreakThrough
  where
    turnsUntilIBreakThrough   = oponentsDefense / (oneIfZero myAttackPerTurn)
    turnsUntilHeBreaksThrough = myDefense / (oneIfZero oponentsAttackPerTurn)

oneIfZero :: Float -> Float
oneIfZero 0 = 1
oneIfZero x = x

attackAndDefensePerRow :: Player -> [(Float, Float)]
attackAndDefensePerRow player =
  map (damageAndDefenseInRow (towerMap player)) [0..width]

damageAndDefenseInRow :: TowerMap -> Int -> (Float, Float)
damageAndDefenseInRow towerMap' y' =
  (damageOfBuildings, healthOfBuildings)
  where
    (healthOfBuildings, damageOfBuildings) = healthAndDamageOfRow y' towerMap'

healthAndDamageOfRow :: Int -> TowerMap -> (Float, Float)
healthAndDamageOfRow y' towerMap' =
  case rowAtIndex y' towerMap' of
    Just aRow -> (foldlRowBuildings' (fromIntegral . integrity) aRow,
                  foldlRowBuildings' damagePerTurn              aRow)
    _         -> (0, 0)

foldlRowBuildings' :: (Building -> Float) -> Row -> Float
foldlRowBuildings' f xs =
  rowFoldl' (accBuilding f) 0 xs

accBuilding :: (Building -> Float) -> Float -> Building -> Float
accBuilding f !summed building' = f building' + summed

missileDamagePerTurn :: Float
missileDamagePerTurn = (fromIntegral missileDamage) / (fromIntegral attackTowerCooldownTime)

damagePerTurn :: Building -> Float
damagePerTurn (Building { buildingType = buildingType' }) =
  if buildingType' /= ATTACK
  then 0
  else missileDamagePerTurn

turnsToTowerMultiplier :: Float
turnsToTowerMultiplier = 1

zeroIfBelow :: Int -> Int
zeroIfBelow x = if x < 0 then 0 else x

mostExpensiveTower :: Int
mostExpensiveTower = maximum [attackTowerCost, defenseTowerCost, energyTowerCost]

infinity :: Int
infinity = 1000000

divWithZero :: Int -> Int -> Int
divWithZero x y =
  if y == 0
  then infinity
  else div x y

turnsToNextTowerByTurnByMultiplier :: Player -> Float
turnsToNextTowerByTurnByMultiplier player =
  (turnsToTowerMultiplier *) $
  fromIntegral $
  divWithZero (zeroIfBelow mostExpensiveTower) totalEnergyPerTurn
  where
    rowsEnergyPerTurn  = rowFoldl' ( \ acc (Building _ _ buildingType') ->
                                       if buildingType' == ENERGY
                                       then acc + energyTowerEnergyGeneratedPerTurn
                                       else acc) 0
    totalEnergyPerTurn =
      (+ energyPerTurn) $
      mapFold (\ row acc -> acc + rowsEnergyPerTurn row) 0 (towerMap player)
