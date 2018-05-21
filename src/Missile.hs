module Missile (tickMissiles, mapMissiles, missilesEmpty, missilesFoldr)
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

moveMissile :: (Int, Int) -> Missile -> SparseMap -> SparseMap
moveMissile (x, y) missile@(Missile { speed = speed', owner = owner' }) gameMap' =
  let adjustment = if owner' == A then speed' else (-speed')
  in adjustAt (addMissile missile) (x, y + adjustment) gameMap'

mapMissiles :: (a -> b) -> V.Vector a -> V.Vector b
mapMissiles = V.map

missilesEmpty :: V.Vector Missile -> Bool
missilesEmpty = V.null

missilesFoldr :: (a -> b -> b) -> b -> V.Vector a -> b
missilesFoldr = V.foldr
