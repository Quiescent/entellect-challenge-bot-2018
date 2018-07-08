module Cell (allCells,
             cellBelongsToMe,
             cellBelongsToOponent)
  where

import Magic
import Coord

allCells :: [Coord]
allCells = [toCoord x y | x <- [0..width - 1], y <- [0..height - 1]]

halfway :: Int
halfway = toCoord (div width 2) 0

cellBelongsToMe :: Coord -> Bool
cellBelongsToMe = (< halfway)

cellBelongsToOponent :: Coord -> Bool
cellBelongsToOponent = (>= halfway)
