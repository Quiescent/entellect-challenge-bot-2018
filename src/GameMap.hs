module GameMap (removeAt,
                mapFoldIndexed,
                definedAt)
  where

import BitSetMap

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

definedAt :: Coord -> BuildingPlacements -> Bool
definedAt = containsBuildingAt

removeAt :: Coord -> BuildingPlacements -> BuildingPlacements
removeAt = removeBuilding
