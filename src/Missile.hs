module Missile (mapMissiles,
                missilesEmpty,
                missilesFoldr,
                oponentsMissile,
                myMissile,
                missilesToList,
                missilesFilter,
                missilesFoldl',
                emptyMissiles,
                consMissile)
  where

import Interpretor (PlayerType(..), GameState(..), Missile(..), CellContents(..), SparseMap)
import GameMap

type Missiles = [Missile]

mapMissiles :: (Missile -> b) -> Missiles -> [b]
mapMissiles = map

missilesEmpty :: Missiles -> Bool
missilesEmpty = null

missilesFoldr :: (Missile -> a -> a) -> a -> Missiles -> a
missilesFoldr = foldr

missilesFoldl' :: (a -> Missile -> a) -> a -> Missiles -> a
missilesFoldl' = foldl

missilesFilter :: (Missile -> Bool) -> Missiles -> Missiles
missilesFilter = filter

missilesToList :: Missiles -> Missiles
missilesToList = id

missileOwnedBy :: (PlayerType -> Bool) -> Missile -> Bool
missileOwnedBy p = p . owner

myMissile :: Missile -> Bool
myMissile = missileOwnedBy (== A)

oponentsMissile :: Missile -> Bool
oponentsMissile = missileOwnedBy (== B)

emptyMissiles :: Missiles
emptyMissiles = []

consMissile :: Missile -> Missiles -> Missiles
consMissile = (:)
