module Command (doNothingCommand, build)
  where

import Interpretor (BuildingType(..),
                    Command)

doNothingCommand :: Command
doNothingCommand = ""

build :: Int -> Int -> BuildingType -> Command
build x y buildingType' =
  show x ++ "," ++ show y ++ "," ++
  case buildingType' of
    DEFENSE -> "0"
    ATTACK  -> "1"
    ENERGY  -> "2"
