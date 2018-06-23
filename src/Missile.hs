module Missile (Direction(..),
               moveMissile)
  where

import Interpretor (Missile(..))

data Direction = MoveLeft | MoveRight
  deriving (Eq)

moveMissile :: Direction -> Missile -> Missile
moveMissile direction missile@(Missile { speed = speed', xDisp = xDisp' }) =
  let adjustment = if direction == MoveRight then speed' else (-speed')
      newX       = xDisp' + adjustment
  in missile { xDisp = newX }
