module Engine (tickEngine)
  where

import Interpretor (GameState(..), Player(..))

import Player
import Building
import GameState
import Magic

tickEngine :: GameState -> GameState
tickEngine = incrementRound . gainEnergy . collideMissiles . tickBuildings

incrementRound :: GameState -> GameState
incrementRound gameState@(GameState { gameRound = gameRound' }) =
  gameState { gameRound = gameRound' + 1 }

gainEnergy :: GameState -> GameState
gainEnergy = mapMyPlayer (incrementEnergy) . mapOponentsPlayer (incrementEnergy)

incrementEnergy :: Player -> Player
incrementEnergy player' = updateEnergy (energyPerTurn + (energyGenPerTurn player')) player'

collideMissiles :: GameState -> GameState
collideMissiles = collideAndMoveMissiles
