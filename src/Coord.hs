module Coord (Coord,
              toCoord,
              fromCoord,
              getY,
              getX,
              halfWay)
  where

import Magic
import VectorIndex

import qualified Data.Vector.Unboxed as UV

type Coord = Int

halfWay :: Int
halfWay = width `div` 2

mySideSizeMaxIndex :: Int
mySideSizeMaxIndex = halfWay * height - 1

toCoord :: Int -> Int -> Coord
toCoord x y = mySideSizeMaxIndex - (y * halfWay + (if x >= halfWay then (width - 1) - x else x))

fromCoord :: Coord -> (Int, Int)
fromCoord coord = (getX coord,
                   getY coord)

correspondingXs :: UV.Vector Int
correspondingXs = UV.fromList $ concat $ replicate 8 [0..halfWay - 1]

correspondingYs :: UV.Vector Int
correspondingYs = UV.fromList $ [0..height - 1] >>= (replicate halfWay)

getY :: Coord -> Int
getY coord = correspondingYs `uVectorIndex` (mySideSizeMaxIndex - coord)

getX :: Coord -> Int
getX coord = correspondingXs `uVectorIndex` (mySideSizeMaxIndex - coord)
