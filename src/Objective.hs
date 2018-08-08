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

import qualified Data.List           as L
import qualified Data.IntMap.Strict  as M
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
  state@(GameState { me = (Player { attackTowersPerRow = myAttackTowersPerRow,
                                    energyTowersPerRow = myEnergyTowersPerRow }),
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
