module Engine (tickEngine)
  where

import Interpretor (GameState(..), Player(..))

import Player
import Building
import GameState
import Magic

tickEngine :: GameState -> GameState
tickEngine = gainEnergy . collideMissiles . tickBuildings

gainEnergy :: GameState -> GameState
gainEnergy = mapMyPlayer (incrementEnergy) . mapOponentsPlayer (incrementEnergy)

incrementEnergy :: Player -> Player
incrementEnergy player' = updateEnergy (energyPerTurn + (energyGenPerTurn player')) player'

-- TODO: Implement moving and coliding the missiles
collideMissiles :: GameState -> GameState
collideMissiles = id
