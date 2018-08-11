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
myIntermediateBoardScore state =
  turnsToMostExpensiveByMostExpensive state +
  howMuchIAttackHisEnergyAndAttackTowers state

howMuchIAttackHisEnergyAndAttackTowers :: GameState -> Float
howMuchIAttackHisEnergyAndAttackTowers
  (GameState { me      = (Player { attackTowersUnderConstruction = myAttackTowersUnderConstruction',
                                   attack3Towers                 = myAttack3Towers',
                                   attack2Towers                 = myAttack2Towers',
                                   attack1Towers                 = myAttack1Towers',
                                   attack0Towers                 = myAttack0Towers' }),
               oponent = (Player { energyTowersUnderConstruction = energyTowersUnderConstruction',
                                   energyTowers                  = energyTowers',
                                   attackTowersUnderConstruction = attackTowersUnderConstruction',
                                   attack3Towers                 = attack3Towers',
                                   attack2Towers                 = attack2Towers',
                                   attack1Towers                 = attack1Towers',
                                   attack0Towers                 = attack0Towers' })}) =
  let allMyAttack                = addAllBuildings myAttackTowersUnderConstruction'
                                   (addAllBuildings myAttack3Towers'
                                     (addAllBuildings myAttack2Towers'
                                      (addAllBuildings myAttack1Towers'
                                       myAttack0Towers')))
      allOponentsAttackAndEnergy = addAllBuildings energyTowersUnderConstruction'
                                   (addAllBuildings energyTowers'
                                    (addAllBuildings attackTowersUnderConstruction'
                                     (addAllBuildings attack3Towers'
                                      (addAllBuildings attack2Towers'
                                       (addAllBuildings attack1Towers'
                                        attack0Towers')))))
  in fromIntegral ((countBuildings (onlyOverlappingBuildings allMyAttack                row0) *
                    countBuildings (onlyOverlappingBuildings allOponentsAttackAndEnergy row0)) +
                   (countBuildings (onlyOverlappingBuildings allMyAttack                row1) *
                    countBuildings (onlyOverlappingBuildings allOponentsAttackAndEnergy row1)) +
                   (countBuildings (onlyOverlappingBuildings allMyAttack                row2) *
                    countBuildings (onlyOverlappingBuildings allOponentsAttackAndEnergy row2)) +
                   (countBuildings (onlyOverlappingBuildings allMyAttack                row3) *
                    countBuildings (onlyOverlappingBuildings allOponentsAttackAndEnergy row3)) +
                   (countBuildings (onlyOverlappingBuildings allMyAttack                row4) *
                    countBuildings (onlyOverlappingBuildings allOponentsAttackAndEnergy row4)) +
                   (countBuildings (onlyOverlappingBuildings allMyAttack                row5) *
                    countBuildings (onlyOverlappingBuildings allOponentsAttackAndEnergy row5)) +
                   (countBuildings (onlyOverlappingBuildings allMyAttack                row6) *
                    countBuildings (onlyOverlappingBuildings allOponentsAttackAndEnergy row6)) +
                   (countBuildings (onlyOverlappingBuildings allMyAttack                row7) *
                    countBuildings (onlyOverlappingBuildings allOponentsAttackAndEnergy row7)))

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
