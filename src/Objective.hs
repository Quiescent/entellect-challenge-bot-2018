{-# LANGUAGE BangPatterns #-}

module Objective (myIntermediateBoardScore,
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

myIntermediateBoardScore :: GameState -> Float
myIntermediateBoardScore state =
  let futureState = advanceToFutureState state
  in energyTowersDestroyed state futureState +
     attackPowerDestroyed state futureState

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

energyTowersDestroyed :: GameState -> GameState -> Float
energyTowersDestroyed
  (GameState { oponent = (Player { energyTowers                  = initialEnergyTowers,
                                   energyTowersUnderConstruction = energyTowersUnderConstruction' }) })
  (GameState { oponent = (Player { energyTowers                  = energyTowersAfter }) }) =
  fromIntegral $ (countBuildings initialEnergyTowers +
                  countBuildings energyTowersUnderConstruction') -
                 (countBuildings energyTowersAfter)

attackPowerDestroyed :: GameState -> GameState -> Float
attackPowerDestroyed
  (GameState {
      oponent = (Player { attackTowersUnderConstruction = attackTowersUnderConstruction',
                          attack3Towers                 = initialAttack3Towers,
                          attack2Towers                 = initialAttack2Towers,
                          attack1Towers                 = initialAttack1Towers,
                          attack0Towers                 = initialAttack0Towers }) })
  (GameState {
      oponent = (Player { attack3Towers                 = attack3TowersAfter,
                          attack2Towers                 = attack2TowersAfter,
                          attack1Towers                 = attack1TowersAfter,
                          attack0Towers                 = attack0TowersAfter }) }) =
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
