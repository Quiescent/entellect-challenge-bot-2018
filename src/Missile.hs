module Missile (tickMissiles, mapMissiles, missilesEmpty, missilesFoldr)
  where

import Interpretor (GameState(..), Missile(..), CellContents(..), SparseMap)
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

-- TODO which direction is the missile going?
moveMissile :: (Int, Int) -> Missile -> SparseMap -> SparseMap
moveMissile (x, y) missile@(Missile { speed = speed' }) gameMap' =
  adjustAt (addMissile missile) (x, y + speed') gameMap'

mapMissiles :: (a -> b) -> V.Vector a -> V.Vector b
mapMissiles = V.map

missilesEmpty :: V.Vector Missile -> Bool
missilesEmpty = V.null

missilesFoldr :: (a -> b -> b) -> b -> V.Vector a -> b
missilesFoldr = V.foldr
