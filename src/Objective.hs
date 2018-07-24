{-# LANGUAGE BangPatterns #-}

module Objective (myBoardScore, Move(..))
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

myBoardScore :: GameState -> Float
myBoardScore state =
  energyProductionDeficitScore state +
  turnsToMostExpensiveByMostExpensive (me state) +
  hitsDealtToOponent state +
  hitsTakenByMe state +
  resultBonus state

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
hitsTakenByMe = (1 -) . normaliseByHitsTaken . fromIntegral . hitsTaken . myPlayer

energyProductionDeficitScore :: GameState -> Float
energyProductionDeficitScore (GameState (Player { energyGenPerTurn = myEnergyPerTurn })
                                        (Player { energyGenPerTurn = oponentsEnergyPerTurn })) =
  (mostExpensiveTower + (fromIntegral (myEnergyPerTurn - oponentsEnergyPerTurn))) / (2 * mostExpensiveTower)

mostExpensiveTower :: Float
mostExpensiveTower = fromIntegral $ maximum [attackTowerCost, defenseTowerCost, energyTowerCost]

minBetweenEnergyPerTurnAndMostExpensive :: Float -> Float
minBetweenEnergyPerTurnAndMostExpensive myEnergyPerTurn =
  if myEnergyPerTurn >= mostExpensiveTower
  then 0
  else (mostExpensiveTower / myEnergyPerTurn) / maxTurnsToNextTower

maxTurnsToNextTower :: Float
maxTurnsToNextTower = mostExpensiveTower / (fromIntegral energyPerTurn)

turnsToMostExpensiveByMostExpensive :: Player -> Float
turnsToMostExpensiveByMostExpensive =
  minBetweenEnergyPerTurnAndMostExpensive .
  (+ (fromIntegral energyPerTurn)) .
  fromIntegral .
  energyGenPerTurn
