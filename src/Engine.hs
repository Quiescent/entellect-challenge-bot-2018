module Engine (tickEngine)
  where

import Interpretor (PlayerType(..),
                    GameState(..),
                    GameDetails(..),
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
  let energyPerTurn = roundIncomeEnergy $ gameDetails state
  in case building'
     of Just (Building {energyGeneratedPerTurn = energyGeneratedPerTurn'}) ->
          if cellBelongsToMe state coordinate
          then (energyPerTurn + myEnergy' + energyGeneratedPerTurn',
                energyPerTurn + oponentsEnergy')
          else (energyPerTurn + myEnergy',
                energyPerTurn + oponentsEnergy' + energyGeneratedPerTurn')
        Nothing -> (energyPerTurn + myEnergy', energyPerTurn + oponentsEnergy')

-- TODO Keep track of hits to player
-- TODO Keep track of score
collideMissiles :: GameState -> GameState
collideMissiles state@(GameState { gameDetails = gameDetails' }) =
  state { gameMap = gameMap' }
  where
    (gameMap', collisions) = foldr (collide (mapWidth gameDetails')) ((gameMap state), []) contentsWithCoords
    contentsWithCoords = mapContentsWithCoords state

data CollisionType = HitPlayer | HitBuilding

data Collision = Collision CollisionType
                           SparseMap
                           PlayerType

collide :: Int -> ((Int, Int), CellContents) -> (SparseMap, [Collision]) -> (SparseMap, [Collision])
collide width ((x, y), (CellContents _ missiles)) (gameMap', collisions)
  | missilesEmpty missiles = (gameMap', collisions)
  | otherwise              = missilesFoldr (checkCollision x y) (gameMap', collisions) missiles
    where
      checkCollision x' y' missile@(Missile damage' speed' owner' _ _) (gameMap'', collisions') =
        let collisionResult = iterCollide (y' - speed') speed' gameMap''
        in case collisionResult of
          Just collision@(Collision _ newMap _) -> (adjustAt (removeMissile missile)
                                                    (x', y')
                                                    newMap,
                                                    collision : collisions')
          Nothing       -> (gameMap'', collisions')
        where
          missileDisp = if owner' == A then 1 else (-1)
          iterCollide :: Int -> Int -> SparseMap -> Maybe Collision
          iterCollide _   0         _          = Nothing
          iterCollide y'' remaining gameMap''' =
            case getAt (x', y'') gameMap''' of
              Just (CellContents (Just building') _) ->
                let adjustedMap = adjustAt (damageBuilding damage')
                                           (x, y'')
                                           gameMap'''
                in Just (Collision HitBuilding adjustedMap (buildingOwner building'))
              _                                      ->
                let playerCollidedWith = if x' == -1
                                         then Just A
                                         else if x' == width
                                              then Just B
                                              else Nothing
                in case playerCollidedWith of
                  (Just player) -> Just (Collision HitPlayer gameMap'' player)
                  _             -> iterCollide (y'' + missileDisp) (remaining - 1) gameMap'''

damageBuilding :: Int -> CellContents -> CellContents
damageBuilding damage' cellContents =
  let (Just building') = buildingInCell cellContents
      integrity'       = integrity building'
  in if integrity' <= damage'
     then cellContents { buildingInCell = Nothing }
     else cellContents { buildingInCell = Just (building' { integrity = integrity' - damage' }) }
