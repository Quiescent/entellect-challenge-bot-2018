module Cell (allCells,
             cellBelongsToMe,
             cellBelongsToOponent)
  where

import Magic

allCells :: [(Int, Int)]
allCells = [(x, y) | x <- [0..width - 1], y <- [0..height - 1]]

halfway :: Int
halfway = div width 2

cellBelongsToMe :: (Int, Int) -> Bool
cellBelongsToMe (x', _) = x' < halfway

cellBelongsToOponent :: (Int, Int) -> Bool
cellBelongsToOponent (x', _) = x' >= halfway
