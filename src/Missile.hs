module Missile (tickMissiles, mapMissiles, missilesEmpty, missilesFoldr)
  where

import Interpretor (GameState(..), Missile(..))
import Data.Vector as V

tickMissiles :: GameState -> GameState
tickMissiles state = state

mapMissiles :: (a -> b) -> V.Vector a -> V.Vector b
mapMissiles = V.map

missilesEmpty :: V.Vector Missile -> Bool
missilesEmpty = V.null

missilesFoldr :: (a -> b -> b) -> b -> V.Vector a -> b
missilesFoldr = V.foldr
