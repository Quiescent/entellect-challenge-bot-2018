module Engine (tickEngine)
  where

import Interpretor (decrementFitness,
                    GameState(..),
                    BuildingType(..),
                    Player(..),
                    Building(..),
                    Missile)
import Player
import Missile
import Building
import GameMap
import GameState
import Magic
import Coord
import BitSetMap

import Control.Monad.State.Lazy

tickEngine :: GameState -> GameState
tickEngine = gainEnergy . collideMissiles . tickBuildings

gainEnergy :: GameState -> GameState
gainEnergy = mapMyPlayer (incrementEnergy) . mapOponentsPlayer (incrementEnergy)

incrementEnergy :: Player -> Player
incrementEnergy player' = updateEnergy (energyPerTurn + (energyGenPerTurn player')) player'

-- TODO: Implement moving and coliding the missiles
collideMissiles :: GameState -> GameState
collideMissiles = id
