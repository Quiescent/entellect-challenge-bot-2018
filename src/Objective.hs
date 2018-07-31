{-# LANGUAGE BangPatterns #-}

module Objective (myIntermediateBoardScore,
                  resultBonusScore,
                  Move(..))
  where

import Interpretor (GameState(..),
                    Command(..),
                    Player(..))
import Player
import Magic
import Engine

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
  let futureState = advanceToFutureState state
  in turnsToMostExpensiveByMostExpensive state

-- Unrolled for the compiler to optimise (there are 10 right now)
advanceToFutureState :: GameState -> GameState
advanceToFutureState =
  tickEngine .
  tickEngine .
  tickEngine .
  tickEngine .
  tickEngine .
  tickEngine .
  tickEngine .
  tickEngine .
  tickEngine .
  tickEngine

resultBonusScore :: Float
resultBonusScore = 9

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
  energyGenPerTurn .
  me
