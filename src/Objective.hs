{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Objective (myBoardScore, Move(..))
  where

import Interpretor (GameState(..),
                    Command(..),
                    Player(..))
import Player
import Magic

import qualified Data.List   as L
import qualified Data.IntMap as M
import GHC.Generics (Generic(..))
import Control.DeepSeq

data Move = Move { myMove       :: Command,
                   oponentsMove :: Command }
          deriving (Show, Generic, Eq)

instance NFData Move

myBoardScore :: GameState -> Float
myBoardScore state =
  hitsSubtractTakenAfterTime state +
  hitsDealtToOponent state -
  hitsTakenByMe state -
  (zeroIfEnoughEnergy (me state) $ turnsToNextTowerByTurn (me state)) +
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
hitsTakenByMe = normaliseByHitsTaken . fromIntegral . hitsTaken . myPlayer

normaliseHitsTakenAfterTime :: Float -> Float
normaliseHitsTakenAfterTime = (/ (2.0 * (fromIntegral height)))

hitsSubtractTakenAfterTime :: GameState -> Float
hitsSubtractTakenAfterTime (GameState me' oponent') =
  normaliseHitsTakenAfterTime $
  L.foldl' (accDamageAndDefense me' oponent') 0 [0..height]

accDamageAndDefense :: Player -> Player -> Float -> Int -> Float
accDamageAndDefense me' oponent' acc y' =
  acc + matchDefenseToAttack (healthAndDamageOfRow y' me') (healthAndDamageOfRow y' oponent')

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
  (fromIntegral (width * defenseTowerHealth4)) / (fromIntegral missileDamage)

oneIfZero :: Float -> Float
oneIfZero 0 = 1
oneIfZero x = x

healthAndDamageOfRow :: Int -> Player -> (Float, Float)
healthAndDamageOfRow y' (Player { attackPerRow  = attackPerRow',
                                  defensePerRow = defensePerRow' }) =
  (fromIntegral $ M.findWithDefault 0 y' defensePerRow',
   M.findWithDefault 0 y' attackPerRow')

mostExpensiveTower :: Float
mostExpensiveTower = fromIntegral $ maximum [attackTowerCost, defenseTowerCost, energyTowerCost]

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
  fromIntegral .
  energyGenPerTurn

zeroIfEnoughEnergy :: Player -> Float -> Float
zeroIfEnoughEnergy (Player { energy = energy' }) x =
  if (fromIntegral energy') >= mostExpensiveTower
  then 0
  else x
