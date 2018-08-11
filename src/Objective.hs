{-# LANGUAGE BangPatterns #-}

module Objective (myIntermediateBoardScore,
                  Move(..))
  where

import Magic
import Engine
import Interpretor (Command, GameState(..), Player(..))
import BitSetMap

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
myIntermediateBoardScore = turnsToMostExpensiveByMostExpensive

mostExpensiveTower :: Float
mostExpensiveTower = fromIntegral $ maximum [attackTowerCost, defenseTowerCost, energyTowerCost]

minBetweenEnergyPerTurnAndMostExpensive :: Float -> Float
minBetweenEnergyPerTurnAndMostExpensive myEnergyPerTurn =
  if myEnergyPerTurn >= mostExpensiveTower
  then maxTurnsToNextTower
  else maxTurnsToNextTower - (mostExpensiveTower / myEnergyPerTurn)

maxTurnsToNextTower :: Float
maxTurnsToNextTower = mostExpensiveTower / (fromIntegral energyPerTurn)

turnsToMostExpensiveByMostExpensive :: GameState -> Float
turnsToMostExpensiveByMostExpensive =
  minBetweenEnergyPerTurnAndMostExpensive .
  (+ (fromIntegral energyPerTurn)) .
  fromIntegral .
  futureEnergyGenPerTurn .
  me

-- Includes energy towers which are yet to be built
futureEnergyGenPerTurn :: Player -> Int
futureEnergyGenPerTurn player =
  ((* energyTowerEnergyGeneratedPerTurn) $
   countBuildings $
   energyTowersUnderConstruction player) +
  (energyGenPerTurn player)
