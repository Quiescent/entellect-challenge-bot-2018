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
import GameDetails
import Collision (CollisionType(..), Collision(..))

tickEngine :: GameState -> GameState
tickEngine = gainEnergy . collideMissiles . tickMissiles . tickBuildings

gainEnergy :: GameState -> GameState
gainEnergy state =
  updatePointsForEnergy mineAndOponentsEnergy $ updateEnergy state mineAndOponentsEnergy
  where
    mineAndOponentsEnergy = foldGameMap (incrementEnergy state)
                                        (myEnergy state, oponentsEnergy state)
                                        state

updatePointsForEnergy :: (Int, Int) -> GameState -> GameState
updatePointsForEnergy (myEnergy', oponentsEnergy') =
  updatePlayerPointsForEnergy A myEnergy' . updatePlayerPointsForEnergy B oponentsEnergy'

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

collideMissiles :: GameState -> GameState
collideMissiles state@(GameState { gameDetails = gameDetails' }) =
  accountForCollisions (state { gameMap = gameMap' }) collisions
  where
    (gameMap', collisions) = foldr (collide (mapWidth gameDetails')) ((gameMap state), []) contentsWithCoords
    contentsWithCoords = mapContentsWithCoords state

accountForCollisions :: GameState -> [Collision] -> GameState
accountForCollisions state collissions =
  foldr accountForCollision state collissions
  where
    accountForCollision (Collision hitType _ playerHit damage') =
      incrementPlayerHits playerHit . updatePointsForHits playerHit hitType damage'

buildingInCellIsConstructed :: CellContents -> Bool
buildingInCellIsConstructed = buildingPredicate buildingIsConstructed

collide :: Int -> ((Int, Int), CellContents) -> (SparseMap, [Collision]) -> (SparseMap, [Collision])
collide width ((x, y), (CellContents _ missiles)) (gameMap', collisions)
  | missilesEmpty missiles = (gameMap', collisions)
  | otherwise              = missilesFoldr (checkCollision x y) (gameMap', collisions) missiles
    where
      checkCollision x' y' missile@(Missile damage' speed' owner' _ _) (gameMap'', collisions') =
        let collisionResult = iterCollide (y' - speed') speed' gameMap''
        in case collisionResult of
          Just collision@(Collision _ newMap _ _) -> (adjustAt (removeMissile missile)
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
              Just contents@(CellContents (Just building') _) ->
                let adjustedMap = adjustAt (damageBuilding damage')
                                           (x, y'')
                                           gameMap'''
                in if buildingInCellIsConstructed contents
                   then Just (Collision HitBuilding adjustedMap (buildingOwner building') damage')
                   else continue
              _                                               ->
                let playerCollidedWith = if x' == -1
                                         then Just A
                                         else if x' == width
                                              then Just B
                                              else Nothing
                in case playerCollidedWith of
                  (Just player) -> Just (Collision HitPlayer gameMap'' player damage')
                  _             -> continue
            where
              continue = iterCollide (y'' + missileDisp) (remaining - 1) gameMap'''

damageBuilding :: Int -> CellContents -> CellContents
damageBuilding damage' cellContents =
  let (Just building') = buildingInCell cellContents
      integrity'       = integrity building'
  in if integrity' <= damage'
     then cellContents { buildingInCell = Nothing }
     else cellContents { buildingInCell = Just (building' { integrity = integrity' - damage' }) }
