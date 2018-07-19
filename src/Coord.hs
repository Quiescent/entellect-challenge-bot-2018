module Coord (Coord,
              toCoord,
              fromCoord,
              getY)
  where

import Magic

type Coord = Int

toCoord :: Int -> Int -> Coord
toCoord x y = y * width + x

fromCoord :: Coord -> (Int, Int)
fromCoord = flipCoord . (flip divMod) width

getY :: Coord -> Int
getY coord = div coord width

flipCoord :: (Int, Int) -> (Int, Int)
flipCoord (x, y) = (y, x)
