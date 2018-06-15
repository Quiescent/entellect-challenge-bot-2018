module Missile (tickMissiles,
                mapMissiles,
                missilesEmpty,
                missilesFoldr,
                oponentsMissile,
                myMissile,
                missilesToList,
                missilesFilter,
                missilesFoldl')
  where

import Interpretor (PlayerType(..), GameState(..), Missile(..), CellContents(..), SparseMap)
import GameMap
import Cell

import Data.Vector as V
import Prelude as P

tickMissiles :: GameState -> GameState
tickMissiles state@(GameState { gameMap = gameMap' }) =
  state { gameMap = P.foldr moveMissiles gameMap' (mapContentsWithCoords state) }

moveMissiles :: ((Int, Int), CellContents) -> SparseMap -> SparseMap
moveMissiles (coordinate, (CellContents _ missiles')) gameMap'
  | missilesEmpty missiles' = gameMap'
  | otherwise               =
    let withMissilesRemoved = adjustAt removeMissiles coordinate gameMap'
    in missilesFoldr (moveMissile coordinate) withMissilesRemoved missiles'

-- TODO: Clean up empty cells so that the map remains sparse
moveMissile :: (Int, Int) -> Missile -> SparseMap -> SparseMap
moveMissile (x, y) missile@(Missile { speed = speed', owner = owner' }) gameMap' =
  let adjustment = if owner' == A then speed' else (-speed')
      newCoord   = (x, y + adjustment)
      gameMap''  = adjustAt (removeMissile missile) (x, y) gameMap'
  in if definedAt newCoord gameMap'
     then adjustAt (addMissile missile) newCoord gameMap''
     else addAt newCoord (addMissile missile emptyCell) gameMap''

mapMissiles :: (Missile -> b) -> V.Vector Missile -> V.Vector b
mapMissiles = V.map

missilesEmpty :: V.Vector Missile -> Bool
missilesEmpty = V.null

missilesFoldr :: (Missile -> a -> a) -> a -> V.Vector Missile -> a
missilesFoldr = V.foldr

missilesFoldl' :: (a -> Missile -> a) -> a -> V.Vector Missile -> a
missilesFoldl' = V.foldl'

missilesFilter :: (Missile -> Bool) -> V.Vector Missile -> V.Vector Missile
missilesFilter = V.filter

missilesToList :: V.Vector Missile -> [Missile]
missilesToList = V.toList

missileOwnedBy :: (PlayerType -> Bool) -> Missile -> Bool
missileOwnedBy p = p . owner

myMissile :: Missile -> Bool
myMissile = missileOwnedBy (== A)

oponentsMissile :: Missile -> Bool
oponentsMissile = missileOwnedBy (== B)
