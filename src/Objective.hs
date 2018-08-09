{-# LANGUAGE BangPatterns #-}

module Objective (myIntermediateBoardScore,
                  resultBonusScore,
                  Move(..))
  where

import Magic

import Interpretor (Command, GameState(..), Player(..))

import qualified Data.Vector.Unboxed as UV
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
myIntermediateBoardScore
  state@(GameState { me = (Player { attackTowersPerRow = myAttackTowersPerRow }),
                     oponent = (Player { attackTowersPerRow = oponentsAttackTowersPerRow,
                                         energyTowersPerRow = oponentsEnergyTowersPerRow })}) =
  turnsToMostExpensiveByMostExpensive state +
  (fromIntegral $ UV.foldl' (+) 0 $ UV.zipWith (*) myAttackTowersPerRow oponentsEnergyTowersPerRow) +
  (fromIntegral $ UV.foldl' (+) 0 $ UV.zipWith (*) myAttackTowersPerRow oponentsAttackTowersPerRow)


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
