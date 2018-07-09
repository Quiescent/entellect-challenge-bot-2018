{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Objective (myBoardScore, Move(..))
  where

import Interpretor (GameState(..),
                    Command(..),
                    Building(..),
                    BuildingType(..),
                    Player(..),
                    TowerMap)
import GameMap
import Player
import Magic
import BuildingsUnderConstruction
import Coord

import qualified Data.List   as L
import qualified Data.IntMap as M
import GHC.Generics (Generic(..))
import Control.DeepSeq

data Move = Move { myMove       :: Command,
                   oponentsMove :: Command }
          deriving (Show, Generic, Eq)

instance NFData Move

myBoardScore :: (GameState, a) -> (Float, (GameState, a))
myBoardScore withMove@(state, _) =
  (hitsSubtractTakenAfterTime withPlacedBuildings +
   hitsDealtToOponent state -
   hitsTakenByMe state -
   turnsToNextTowerByTurn meWithPlacedBuildings +
   resultBonus state,
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

resultBonus :: GameState -> Float
resultBonus state =
  if myHealth state       == 0 then -100 else 0 +
  if oponentsHealth state == 0 then 100  else 0

maxHitsTaken :: Float
maxHitsTaken = (fromIntegral startingHealth) / (fromIntegral missileDamage)

normaliseByHitsTaken :: Float -> Float
normaliseByHitsTaken = (/ maxHitsTaken)

hitsDealtToOponent :: GameState -> Float
hitsDealtToOponent = normaliseByHitsTaken . fromIntegral . hitsTaken . oponentsPlayer

hitsTakenByMe :: GameState -> Float
hitsTakenByMe = normaliseByHitsTaken . fromIntegral . hitsTaken . myPlayer

normaliseHitsTakenAfterTime :: Float -> Float
normaliseHitsTakenAfterTime = (/ (2.0 * (fromIntegral height)))

hitsSubtractTakenAfterTime :: GameState -> Float
hitsSubtractTakenAfterTime (GameState me' oponent') =
  normaliseHitsTakenAfterTime $
  L.foldl' (accDamageAndDefense myTowerMap oponentsTowerMap) 0 [0..height]
  where
    myTowerMap       = towerMap me'
    oponentsTowerMap = towerMap oponent'

accDamageAndDefense :: TowerMap -> TowerMap -> Float -> Int -> Float
accDamageAndDefense myTowerMap oponentsTowerMap acc y' =
  acc + matchDefenseToAttack (healthAndDamageOfRow y' myTowerMap) (healthAndDamageOfRow y' oponentsTowerMap)

matchDefenseToAttack :: (Float, Float) -> (Float, Float) -> Float
matchDefenseToAttack (myAttackPerTurn, myDefense) (oponentsAttackPerTurn, oponentsDefense) =
  (oponentBreakThroughScore / oponentsAttackingMod) + myBreakThroughScore
  where
    myBreakThroughScore      = 1.0 - (normaliseByMaxTurns $ oponentsDefense / (oneIfZero myAttackPerTurn))
    oponentsAttackingMod     = if oponentsAttackPerTurn == 0 then maxTurnsToBreakThrough else 1
    oponentBreakThroughScore = normaliseByMaxTurns        $ myDefense       / (oneIfZero oponentsAttackPerTurn)

normaliseByMaxTurns :: Float -> Float
normaliseByMaxTurns = (/ maxTurnsToBreakThrough)

maxTurnsToBreakThrough :: Float
maxTurnsToBreakThrough =
  (fromIntegral (width * defenseTowerHealth)) / (fromIntegral missileDamage)

oneIfZero :: Float -> Float
oneIfZero 0 = 1
oneIfZero x = x

healthAndDamageOfRow :: Int -> TowerMap -> (Float, Float)
healthAndDamageOfRow y' towerMap' =
  mapFoldRow y' (fromIntegral . integrity) damagePerTurn towerMap'

mapFoldRow :: Int -> (Building -> Float) -> (Building -> Float) -> TowerMap -> (Float, Float)
mapFoldRow y' f g towerMap' =
  iterMapFoldRow rowStart (0, 0)
  where
    iterMapFoldRow coord (a, b)
      | coord > rowEnd = (a, b)
      | otherwise      =
        case M.lookupGE coord towerMap' of
          Just (coord', building') ->
            if coord' > rowEnd
            then (a, b)
            else iterMapFoldRow (coord' + 1) (f building' + a, g building' + b)
          Nothing -> (a, b)
    rowStart = toCoord 0           y'
    rowEnd   = toCoord (width - 1) y'

missileDamagePerTurn :: Float
missileDamagePerTurn = (fromIntegral missileDamage) / (fromIntegral attackTowerCooldownTime)

damagePerTurn :: Building -> Float
damagePerTurn (Building { buildingType = buildingType' }) =
  if buildingType' /= ATTACK
  then 0
  else missileDamagePerTurn

zeroIfBelow :: Float -> Float
zeroIfBelow x = if x < 0 then 0 else x

mostExpensiveTower :: Float
mostExpensiveTower = fromIntegral $ maximum [attackTowerCost, defenseTowerCost, energyTowerCost]

infinity :: Float
infinity = 1000000

asFractionOfMaximumTurns :: Float -> Float
asFractionOfMaximumTurns myEnergyPerTurn =
  if myEnergyPerTurn >= mostExpensiveTower
  then 0
  else (mostExpensiveTower / myEnergyPerTurn) / maxTurnsToNextTower

maxTurnsToNextTower :: Float
maxTurnsToNextTower = mostExpensiveTower / (fromIntegral energyPerTurn)

turnsToNextTowerByTurn :: Player -> Float
turnsToNextTowerByTurn =
  asFractionOfMaximumTurns .
  (+ (fromIntegral energyPerTurn)) .
  mapFold accEnergy 0 .
  towerMap

accEnergy :: Building -> Float -> Float
accEnergy (Building _ _ ENERGY) acc = acc + (fromIntegral energyTowerEnergyGeneratedPerTurn)
accEnergy _                     acc = acc
