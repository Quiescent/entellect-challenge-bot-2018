module BitSetMap (Missiles,
                  BuildingPlacements,
                  addAllMissiles,
                  emptyBuildings,
                  addAllBuildings,
                  emptyMissiles,
                  addBuilding,
                  containsMissile,
                  addMissile,
                  placementWidth,
                  containsBuildingAt,
                  removeBuilding,
                  moveMissilesLeft,
                  moveMissilesRight,
                  interSectionIndices)
  where

import Data.Word
import Data.Bits

import Coord

type Missiles = Word64

pack :: Coord -> Word64
pack x = 1 `shiftL` x

isSetAt :: Coord -> Word64 -> Bool
isSetAt i x = (pack i) .&. x > 0

setAt :: Coord -> Word64 -> Word64
setAt i x = (pack i) .|. x

unSetAt :: Coord -> Word64 -> Word64
unSetAt i x = (complement (pack i)) .&. x

addAll :: Word64 -> Word64 -> Word64
addAll = (.|.)

containsMissile :: Coord -> Missiles -> Bool
containsMissile = isSetAt

addMissile :: Coord -> Missiles -> Missiles
addMissile = setAt

addAllMissiles :: Missiles -> Missiles -> Missiles
addAllMissiles = addAll

moveMissilesRight :: Missiles -> Missiles
moveMissilesRight = (flip shiftR) 1

moveMissilesLeft :: Missiles -> Missiles
moveMissilesLeft = (flip shiftL) 1

emptyMissiles :: Missiles
emptyMissiles = 0

type BuildingPlacements = Word64

containsBuildingAt :: Coord -> BuildingPlacements -> Bool
containsBuildingAt = isSetAt

addBuilding :: Coord -> BuildingPlacements -> BuildingPlacements
addBuilding = setAt

placementWidth :: Int
placementWidth = 64

removeBuilding :: Coord -> BuildingPlacements -> BuildingPlacements
removeBuilding = unSetAt

addAllBuildings :: BuildingPlacements -> BuildingPlacements -> BuildingPlacements
addAllBuildings = addAll

emptyBuildings :: BuildingPlacements
emptyBuildings = 0

interSectionIndices :: Missiles -> BuildingPlacements -> [Int]
interSectionIndices missiles buildingPlacements =
  go 0
  where
    intersection = missiles .&. buildingPlacements
    go i
      | i == placementWidth = []
      | otherwise =
        let i'   = i + 1
            rest = (go (i' `seq` i'))
        in if isSetAt i intersection
           then i:rest
           else rest
