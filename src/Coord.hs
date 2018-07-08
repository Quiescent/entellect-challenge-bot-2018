module Coord (Coord,
              toCoord,
              fromCoord)
  where

import Magic

type Coord = Int

toCoord :: Int -> Int -> Coord
toCoord x y = y * width + x

fromCoord :: Coord -> (Int, Int)
fromCoord = (flip divMod) width 
