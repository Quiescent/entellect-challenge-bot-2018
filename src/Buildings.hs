module Buildings where

data Building = EnergyTower
              | Defense4
              | Defense3
              | Defense2
              | Defense1
              | Attack3
              | Attack2
              | Attack1
              | Attack0
              | Tesla10
              | Tesla9
              | Tesla8
              | Tesla7
              | Tesla6
              | Tesla5
              | Tesla4
              | Tesla3
              | Tesla2
              | Tesla1
              | Tesla0
  deriving (Enum, Eq, Show)

buildingToInt :: Building -> Int
buildingToInt EnergyTower = 0
buildingToInt Defense4    = 1
buildingToInt Defense3    = 2
buildingToInt Defense2    = 3
buildingToInt Defense1    = 4
buildingToInt Attack3     = 5
buildingToInt Attack2     = 6
buildingToInt Attack1     = 7
buildingToInt Attack0     = 8
buildingToInt Tesla10     = 9
buildingToInt Tesla9      = 10
buildingToInt Tesla8      = 11
buildingToInt Tesla7      = 12
buildingToInt Tesla6      = 13
buildingToInt Tesla5      = 14
buildingToInt Tesla4      = 15
buildingToInt Tesla3      = 16
buildingToInt Tesla2      = 17
buildingToInt Tesla1      = 18
buildingToInt Tesla0      = 19
