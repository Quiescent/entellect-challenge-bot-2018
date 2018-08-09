module GameMap (removeAt,
                mapFoldIndexed,
                definedAt,
                findRightOf,
                findLeftOf,
                CollisionDetector,
                Collision(..))
  where

import Interpretor (Building(..))
import BitSetMap
import Magic

import Coord

mapFoldIndexed :: (Int -> Bool -> a -> a) -> a -> BuildingPlacements -> a
mapFoldIndexed f z xs =
  go 0 z
  where
    go i z'
      | i == placementWidth = z'
      | otherwise           =
        let acc = (f i (containsBuildingAt i xs) z')
            i'  = (i + 1)
        in acc `seq` i' `seq` go i' acc

type CollisionDetector = Missiles -> BuildingPlacements -> Collision

data Collision = HitNothing
               | HitPlayer
               | HitBuilding Int Building
  deriving (Show, Eq)

-- The rules for wrapping are tricky here.  Rely on the play through
-- log functionality to test them.
-- TODO: Implement
findRightOf :: CollisionDetector
findRightOf missiles buildings = HitNothing

firstOutOfBoundsCoord :: Int
firstOutOfBoundsCoord = width * height

findLeftOf :: CollisionDetector
findLeftOf coord buildings = HitNothing

definedAt :: Coord -> BuildingPlacements -> Bool
definedAt = containsBuildingAt

removeAt :: Coord -> BuildingPlacements -> BuildingPlacements
removeAt = removeBuilding
