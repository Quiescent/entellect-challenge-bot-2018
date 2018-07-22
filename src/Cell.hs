module Cell (allCells,
             cellBelongsToMe,
             cellBelongsToOponent)
  where

import Magic
import Coord

import qualified Data.Vector.Unboxed as UV

allCells :: UV.Vector Coord
allCells = UV.fromList [toCoord x y | x <- [0..width - 1], y <- [0..height - 1]]

halfway :: Int
halfway = div width 2

cellBelongsToMe :: Coord -> Bool
cellBelongsToMe coord =
  let (x, _) = fromCoord coord
  in (x < halfway)

cellBelongsToOponent :: Coord -> Bool
cellBelongsToOponent coord =
  let (x, _) = fromCoord coord
  in (x >= halfway)
