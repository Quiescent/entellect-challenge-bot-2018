module Cell (allCells)
  where

import Magic
import Coord

import qualified Data.Vector.Unboxed as UV

allCells :: UV.Vector Coord
allCells = UV.fromList [toCoord x y | x <- [0..halfWay - 1], y <- [0..height - 1]]
