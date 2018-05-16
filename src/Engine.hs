module Engine (tickEngine)
  where

import Interpretor (GameState(..),
                    SparseMap,
                    CellContents(..),
                    Building(..),
                    Missile(..),
                    CellContents(..))
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
  state { gameMap = foldr collide (gameMap state) contentsWithCoords }
  where
    contentsWithCoords = mapContentsWithCoords state

-- TODO remove the missile
collide :: ((Int, Int), CellContents) -> SparseMap -> SparseMap
collide ((x, y), (CellContents _ missiles)) gameMap'
  | missilesEmpty missiles = gameMap'
  | otherwise              = missilesFoldr (checkCollision x y) gameMap' missiles
    where
      checkCollision x' y' (Missile damage' speed') gameMap'' =
        iterCollide (y' - speed') speed' gameMap''
        where
          -- TODO which direction did the missile come from??
          iterCollide :: Int -> Int -> SparseMap -> SparseMap
          iterCollide _   0         gameMap''' = gameMap'''
          iterCollide y'' remaining gameMap''' =
            case getAt (x', y'') gameMap''' of
              Just (CellContents (Just _) _) ->
                let adjustedMap = adjustAt (damageBuilding damage')
                                           (x, y'')
                                           gameMap'''
                in iterCollide (y'' + 1) (remaining - 1) adjustedMap
              _                                      ->
                iterCollide (y'' + 1) (remaining - 1) gameMap'''

damageBuilding :: Int -> CellContents -> CellContents
damageBuilding damage' cellContents =
  let (Just building') = buildingInCell cellContents
      integrity'       = integrity building'
  in if integrity' <= damage'
     then cellContents { buildingInCell = Nothing }
     else cellContents { buildingInCell = Just (building' { integrity = integrity' - damage' }) }
