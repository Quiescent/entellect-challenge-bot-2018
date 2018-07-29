{-# LANGUAGE BangPatterns #-}

module Objective (myIntermediateBoardScore,
                  myFinalBoardScore,
                  resultBonusScore,
                  Move(..))
  where

import Interpretor (GameState(..),
                    Command(..),
                    Player(..))
import Player
import Magic

import qualified Data.List          as L
import qualified Data.IntMap.Strict as M
import Control.DeepSeq

data Move = Move { myMove       :: Command,
                   oponentsMove :: Command }
          deriving (Show, Eq)

instance NFData Move where
  rnf (Move myMove' oponentsMove') =
    (rnf myMove')       `seq`
    (rnf oponentsMove') `seq`
    ()

myIntermediateBoardScore :: GameState -> Float
myIntermediateBoardScore state =
  (1 - hitsDealtToOponent state) +
  (enemyEnergyTowerCountScore state) +
  energyProductionDeficitScore state +
  attackPowerDeficitScore state +
  towerCountDeficitScore state +
  turnsToMostExpensiveByMostExpensive state

myFinalBoardScore :: GameState -> Float
myFinalBoardScore state =
  myIntermediateBoardScore state +
  hitsDealtToOponent state +
  hitsTakenByMe state +
  resultBonus state

resultBonusScore :: Float
resultBonusScore = 9

resultBonus :: GameState -> Float
resultBonus state =
  if myHealth state       == 0 then 0                else 0 +
  if oponentsHealth state == 0 then resultBonusScore else 0

maxHitsTaken :: Float
maxHitsTaken = (fromIntegral startingHealth) / (fromIntegral missileDamage)

normaliseByMaxHitsTaken :: Float -> Float
normaliseByMaxHitsTaken = (/ maxHitsTaken)

hitsDealtToOponent :: GameState -> Float
hitsDealtToOponent = normaliseByMaxHitsTaken . fromIntegral . hitsTaken . oponentsPlayer

hitsTakenByMe :: GameState -> Float
hitsTakenByMe = (1 -) . normaliseByMaxHitsTaken . fromIntegral . hitsTaken . myPlayer

deficitScopeCalculation :: Float -> Float -> Float
deficitScopeCalculation deficit scope =
  if deficit <= (-scope)
  then 0
  else if deficit >= scope
       then 1
       else (scope + deficit)  / (2 * scope)

enemyEnergyTowerCountScore :: GameState -> Float
enemyEnergyTowerCountScore (GameState { oponent = (Player { energyTowerCount = energyTowerCount' }) }) =
  (((fromIntegral width) / 2.0) * (fromIntegral height) - (fromIntegral energyTowerCount')) / towerCountScope

attackPowerScope :: Float
attackPowerScope = 5 * (fromIntegral missileDamage)

attackPowerDeficitScore :: GameState -> Float
attackPowerDeficitScore (GameState (Player { attackPower = myAttackPower })
                                   (Player { attackPower = oponentsAttackPower })) =
  deficitScopeCalculation (fromIntegral (myAttackPower - oponentsAttackPower)) attackPowerScope

towerCountScope :: Float
towerCountScope = ((fromIntegral width) / 2.0) * (fromIntegral height)

towerCountDeficitScore :: GameState -> Float
towerCountDeficitScore (GameState (Player { towerCount = myTowerCount })
                                  (Player { towerCount = oponentsTowerCount })) =
  deficitScopeCalculation (fromIntegral (myTowerCount - oponentsTowerCount)) towerCountScope

energyDeficitScope :: Float
energyDeficitScope = 2 * (fromIntegral energyTowerEnergyGeneratedPerTurn)

energyProductionDeficitScore :: GameState -> Float
energyProductionDeficitScore (GameState (Player { energyGenPerTurn = myEnergyPerTurn })
                                        (Player { energyGenPerTurn = oponentsEnergyPerTurn })) =
  deficitScopeCalculation (fromIntegral (myEnergyPerTurn - oponentsEnergyPerTurn)) energyDeficitScope

mostExpensiveTower :: Float
mostExpensiveTower = fromIntegral $ maximum [attackTowerCost, defenseTowerCost, energyTowerCost]

minBetweenEnergyPerTurnAndMostExpensive :: Float -> Float
minBetweenEnergyPerTurnAndMostExpensive myEnergyPerTurn =
  if myEnergyPerTurn >= mostExpensiveTower
  then 0
  else (mostExpensiveTower / myEnergyPerTurn) / maxTurnsToNextTower

maxTurnsToNextTower :: Float
maxTurnsToNextTower = mostExpensiveTower / (fromIntegral energyPerTurn)

turnsToMostExpensiveByMostExpensive :: GameState -> Float
turnsToMostExpensiveByMostExpensive =
  minBetweenEnergyPerTurnAndMostExpensive .
  (+ (fromIntegral energyPerTurn)) .
  fromIntegral .
  energyGenPerTurn .
  me
