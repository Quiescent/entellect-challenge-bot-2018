{-# LANGUAGE BangPatterns #-}

module Objective (oponentsIntermediateBoardScore,
                  myIntermediateBoardScore,
                  Move(..))
  where

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

oponentsIntermediateBoardScore :: GameState -> Float
oponentsIntermediateBoardScore state =
  let futureState = advanceToFutureState state
  in energyTowersDestroyed (me      state) (me      futureState) +
     attackPowerDestroyed  (me      state) (me      futureState) -
     energyTowersDestroyed (oponent state) (oponent futureState)

myIntermediateBoardScore :: GameState -> Float
myIntermediateBoardScore state =
  let futureState = advanceToFutureState state
  in energyTowersDestroyed (oponent state) (oponent futureState) +
     attackPowerDestroyed  (oponent state) (oponent futureState) -
     energyTowersDestroyed (me      state) (me      futureState)

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

energyTowersDestroyed :: Player -> Player -> Float
energyTowersDestroyed
  (Player { energyTowers                  = initialEnergyTowers,
            energyTowersUnderConstruction = energyTowersUnderConstruction' })
  (Player { energyTowers                  = energyTowersAfter }) =
  fromIntegral $ (countBuildings initialEnergyTowers +
                  countBuildings energyTowersUnderConstruction') -
                 (countBuildings energyTowersAfter)

attackPowerDestroyed :: Player -> Player -> Float
attackPowerDestroyed
  (Player { attackTowersUnderConstruction = attackTowersUnderConstruction',
            attack3Towers                 = initialAttack3Towers,
            attack2Towers                 = initialAttack2Towers,
            attack1Towers                 = initialAttack1Towers,
            attack0Towers                 = initialAttack0Towers })
  (Player { attack3Towers                 = attack3TowersAfter,
            attack2Towers                 = attack2TowersAfter,
            attack1Towers                 = attack1TowersAfter,
            attack0Towers                 = attack0TowersAfter }) =
  fromIntegral $ (countBuildings
                  (addAllBuildings attackTowersUnderConstruction'
                   (addAllBuildings initialAttack3Towers
                    (addAllBuildings initialAttack2Towers
                     (addAllBuildings initialAttack1Towers
                      initialAttack0Towers))))) -
                  (countBuildings
                   (addAllBuildings attack3TowersAfter
                    (addAllBuildings attack2TowersAfter
                     (addAllBuildings attack1TowersAfter
                      attack0TowersAfter))))
