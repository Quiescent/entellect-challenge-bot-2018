module Coord (Coord,
              toCoord,
              fromCoord,
              getY,
              getX)
  where

import Magic
import VectorIndex

import qualified Data.Vector.Unboxed as UV

type Coord = Int

toCoord :: Int -> Int -> Coord
toCoord x y = y * width + x

fromCoord :: Coord -> (Int, Int)
fromCoord coord = (getX coord,
                   getY coord)

correspondingXs :: UV.Vector Int
correspondingXs = UV.fromList $ concat $ replicate 8 [0..width - 1]

correspondingYs :: UV.Vector Int
correspondingYs = UV.fromList $ [0..height - 1] >>= (replicate width)

getY :: Coord -> Int
getY coord = correspondingYs `uVectorIndex` coord

getX :: Coord -> Int
getX coord = correspondingXs `uVectorIndex` coord
