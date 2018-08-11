module BitSetMap (Missiles,
                  BuildingPlacements,
                  buildingPlacementsAreEmpty,
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
                  onlyOverlappingMissiles,
                  removeAllBuildings,
                  missilesWhichCollided,
                  removeAllMissiles,
                  buildingPlacementDifference,
                  missilesAboutToHitPlayer,
                  missilesAboutToTransfer,
                  countMissiles,
                  countBuildings,
                  onlyOverlappingBuildings,
                  row0,
                  row1,
                  row2,
                  row3,
                  row4,
                  row5,
                  row6,
                  row7)
  where

import Data.Word
import Data.Bits

import Coord

notEmpty :: Word64 -> Bool
notEmpty = (== 0)

isSetAt :: Coord -> Word64 -> Bool
isSetAt = flip testBit

setAt :: Coord -> Word64 -> Word64
setAt = flip setBit

unSetAt :: Coord -> Word64 -> Word64
unSetAt = flip clearBit

addAll :: Word64 -> Word64 -> Word64
addAll = (.|.)

removeAll :: Word64 -> Word64 -> Word64
removeAll xs ys = (complement xs) .&. ys

difference :: Word64 -> Word64 -> Word64
difference = xor

count :: Word64 -> Int
count = popCount

type Missiles = Word64

countMissiles :: Missiles -> Int
countMissiles = count

missilesAboutToTransfer :: Missiles
missilesAboutToTransfer = 72340172838076673
-- 0000000100000001000000010000000100000001000000010000000100000001

missilesAboutToHitPlayer :: Missiles
missilesAboutToHitPlayer = 9259542123273814144
-- 1000000010000000100000001000000010000000100000001000000010000000

onlyOverlappingMissiles :: Missiles -> Missiles -> Missiles
onlyOverlappingMissiles = (.&.)

missilesWhichCollided :: Missiles -> BuildingPlacements -> Missiles
missilesWhichCollided = (.&.)

removeAllMissiles :: Missiles -> Missiles -> Missiles
removeAllMissiles = removeAll
  
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

countBuildings :: BuildingPlacements -> Int
countBuildings = count

buildingPlacementsAreEmpty :: BuildingPlacements -> Bool
buildingPlacementsAreEmpty = notEmpty

containsBuildingAt :: Coord -> BuildingPlacements -> Bool
containsBuildingAt = isSetAt

addBuilding :: Coord -> BuildingPlacements -> BuildingPlacements
addBuilding = setAt

placementWidth :: Int
placementWidth = 64

removeBuilding :: Coord -> BuildingPlacements -> BuildingPlacements
removeBuilding = unSetAt

removeAllBuildings :: BuildingPlacements -> BuildingPlacements -> BuildingPlacements
removeAllBuildings = removeAll

addAllBuildings :: BuildingPlacements -> BuildingPlacements -> BuildingPlacements
addAllBuildings = addAll

buildingPlacementDifference :: BuildingPlacements -> BuildingPlacements -> BuildingPlacements
buildingPlacementDifference = difference

onlyOverlappingBuildings :: BuildingPlacements -> BuildingPlacements -> BuildingPlacements
onlyOverlappingBuildings = (.&.)

emptyBuildings :: BuildingPlacements
emptyBuildings = 0

-- Rows:

row0 :: Word64
row0 = 255

row1 :: Word64
row1 = row0 `shiftL` halfWay

row2 :: Word64
row2 = row0 `shiftL` (halfWay * 2)

row3 :: Word64
row3 = row0 `shiftL` (halfWay * 3)

row4 :: Word64
row4 = row0 `shiftL` (halfWay * 4)

row5 :: Word64
row5 = row0 `shiftL` (halfWay * 5)

row6 :: Word64
row6 = row0 `shiftL` (halfWay * 6)

row7 :: Word64
row7 = row0 `shiftL` (halfWay * 7)
