module Collision (CollisionType(..), Collision(..))
  where

import Interpretor(PlayerType(..), SparseMap)

data CollisionType = HitPlayer | HitBuilding

-- Always the player who was hit
data Collision = Collision CollisionType
                           SparseMap
                           PlayerType
                           Int
