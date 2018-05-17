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

-- TODO Remove missiles which collide with the player
-- TODO Keep track of hits to player
-- TODO Keep track of score
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
      -- This tells me if this missile collided but doesn't allow me
      -- to remove it...
      checkCollision x' y' missile@(Missile damage' speed' owner' _ _) gameMap'' =
        let collisionResult = iterCollide (y' - speed') speed' gameMap''
        in case collisionResult of
          Just (newMap) -> newMap
          Nothing       -> gameMap''
        where
          -- TODO which direction did the missile come from??
          iterCollide :: Int -> Int -> SparseMap -> Maybe SparseMap
          iterCollide _   0         _          = Nothing
          iterCollide y'' remaining gameMap''' =
            case getAt (x', y'') gameMap''' of
              Just (CellContents (Just _) _) ->
                let adjustedMap = adjustAt (damageBuilding damage')
                                           (x, y'')
                                           gameMap'''
                in Just adjustedMap
              _                                      ->
                iterCollide (y'' + 1) (remaining - 1) gameMap'''

damageBuilding :: Int -> CellContents -> CellContents
damageBuilding damage' cellContents =
  let (Just building') = buildingInCell cellContents
      integrity'       = integrity building'
  in if integrity' <= damage'
     then cellContents { buildingInCell = Nothing }
     else cellContents { buildingInCell = Just (building' { integrity = integrity' - damage' }) }
