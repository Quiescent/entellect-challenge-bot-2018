module Engine (tickEngine)
  where

import Interpretor (GameState(..),
                    SparseMap,
                    CellContents(..),
                    Building(..))
import Cell
import Player
import Missile
import Building
import GameMap

tickEngine :: GameState -> GameState
tickEngine = gainEnergy . collideMissiles . tickMissiles . tickBuildings

-- TODO learn energy per turn
energyPerTurn :: Int
energyPerTurn = 10

gainEnergy :: GameState -> GameState
gainEnergy state =
  updateEnergy state mineAndOponentsEnergy
  where
    mineAndOponentsEnergy = foldGameMap (incrementEnergy state)
                                        (myEnergy state, oponentsEnergy state)
                                        state

incrementEnergy :: GameState -> (Int, Int) -> CellContents -> (Int, Int) -> (Int, Int)
incrementEnergy state
                coordinate
                (CellContents { buildingInCell = building' })
                (myEnergy', oponentsEnergy') =
  case building'
  of Just (Building {energyGeneratedPerTurn = energyGeneratedPerTurn'}) ->
       if cellBelongsToMe state coordinate
       then (energyPerTurn + myEnergy' + energyGeneratedPerTurn',
             energyPerTurn + oponentsEnergy')
       else (energyPerTurn + myEnergy',
             energyPerTurn + oponentsEnergy' + energyGeneratedPerTurn')
     Nothing -> (energyPerTurn + myEnergy', energyPerTurn + oponentsEnergy')


collideMissiles :: GameState -> GameState
collideMissiles state =
  state { gameMap = foldr collide (gameMap state) contents }
  where
    contents = mapContents state

collide :: CellContents -> SparseMap -> SparseMap
collide contents' gameMap' = gameMap'
