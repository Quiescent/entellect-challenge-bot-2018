module Coord (Coord,
              toCoord,
              fromCoord)
  where

import Magic

type Coord = Int

toCoord :: Int -> Int -> Coord
toCoord x y = y * width + x

fromCoord :: Coord -> (Int, Int)
fromCoord = flipCoord . (flip divMod) width

flipCoord :: (Int, Int) -> (Int, Int)
flipCoord (x, y) = (y, x)
