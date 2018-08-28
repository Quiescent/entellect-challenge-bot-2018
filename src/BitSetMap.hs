{-# LANGUAGE BangPatterns #-}

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
                  fullBoard,
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
notEmpty !x = x == 0

isSetAt :: Coord -> Word64 -> Bool
isSetAt !coord !word = testBit word coord

setAt :: Coord -> Word64 -> Word64
setAt !coord !word = setBit word coord

unSetAt :: Coord -> Word64 -> Word64
unSetAt !coord !word = clearBit word coord

addAll :: Word64 -> Word64 -> Word64
addAll !x !y = x .|. y

removeAll :: Word64 -> Word64 -> Word64
removeAll !xs !ys = (complement xs) .&. ys

difference :: Word64 -> Word64 -> Word64
difference !x !y = x `xor` y

count :: Word64 -> Int
count !x = popCount x

intersection :: Word64 -> Word64 -> Word64
intersection !x !y = x .&. y

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
onlyOverlappingMissiles = intersection

missilesWhichCollided :: Missiles -> BuildingPlacements -> Missiles
missilesWhichCollided = intersection

removeAllMissiles :: Missiles -> Missiles -> Missiles
removeAllMissiles = removeAll
  
containsMissile :: Coord -> Missiles -> Bool
containsMissile = isSetAt

addMissile :: Coord -> Missiles -> Missiles
addMissile = setAt

addAllMissiles :: Missiles -> Missiles -> Missiles
addAllMissiles = addAll

moveMissilesRight :: Missiles -> Missiles
moveMissilesRight !missiles = shiftR missiles 1

moveMissilesLeft :: Missiles -> Missiles
moveMissilesLeft !missiles = shiftL missiles 1

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
onlyOverlappingBuildings = intersection

emptyBuildings :: BuildingPlacements
emptyBuildings = 0

-- Rows:

row0 :: Word64
row0 = addBuilding (toCoord 0 0)
       (addBuilding (toCoord 1 0)
        (addBuilding (toCoord 2 0)
         (addBuilding (toCoord 3 0)
          (addBuilding (toCoord 4 0)
           (addBuilding (toCoord 5 0)
            (addBuilding (toCoord 6 0)
             (addBuilding (toCoord 7 0) 0)))))))

row1 :: Word64
row1 = addBuilding (toCoord 0 1)
       (addBuilding (toCoord 1 1)
        (addBuilding (toCoord 2 1)
         (addBuilding (toCoord 3 1)
          (addBuilding (toCoord 4 1)
           (addBuilding (toCoord 5 1)
            (addBuilding (toCoord 6 1)
             (addBuilding (toCoord 7 1) 0)))))))

row2 :: Word64
row2 = addBuilding (toCoord 0 2)
       (addBuilding (toCoord 1 2)
        (addBuilding (toCoord 2 2)
         (addBuilding (toCoord 3 2)
          (addBuilding (toCoord 4 2)
           (addBuilding (toCoord 5 2)
            (addBuilding (toCoord 6 2)
             (addBuilding (toCoord 7 2) 0)))))))

row3 :: Word64
row3 = addBuilding (toCoord 0 3)
       (addBuilding (toCoord 1 3)
        (addBuilding (toCoord 2 3)
         (addBuilding (toCoord 3 3)
          (addBuilding (toCoord 4 3)
           (addBuilding (toCoord 5 3)
            (addBuilding (toCoord 6 3)
             (addBuilding (toCoord 7 3) 0)))))))

row4 :: Word64
row4 = addBuilding (toCoord 0 4)
       (addBuilding (toCoord 1 4)
        (addBuilding (toCoord 2 4)
         (addBuilding (toCoord 3 4)
          (addBuilding (toCoord 4 4)
           (addBuilding (toCoord 5 4)
            (addBuilding (toCoord 6 4)
             (addBuilding (toCoord 7 4) 0)))))))

row5 :: Word64
row5 = addBuilding (toCoord 0 5)
       (addBuilding (toCoord 1 5)
        (addBuilding (toCoord 2 5)
         (addBuilding (toCoord 3 5)
          (addBuilding (toCoord 4 5)
           (addBuilding (toCoord 5 5)
            (addBuilding (toCoord 6 5)
             (addBuilding (toCoord 7 5) 0)))))))

row6 :: Word64
row6 = addBuilding (toCoord 0 6)
       (addBuilding (toCoord 1 6)
        (addBuilding (toCoord 2 6)
         (addBuilding (toCoord 3 6)
          (addBuilding (toCoord 4 6)
           (addBuilding (toCoord 5 6)
            (addBuilding (toCoord 6 6)
             (addBuilding (toCoord 7 6) 0)))))))

row7 :: Word64
row7 = addBuilding (toCoord 0 7)
       (addBuilding (toCoord 1 7)
        (addBuilding (toCoord 2 7)
         (addBuilding (toCoord 3 7)
          (addBuilding (toCoord 4 7)
           (addBuilding (toCoord 5 7)
            (addBuilding (toCoord 6 7)
             (addBuilding (toCoord 7 7) 0)))))))

-- Boards:

fullBoard :: Word64
fullBoard = 18446744073709551615
