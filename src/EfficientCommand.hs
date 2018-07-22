module EfficientCommand where

import Interpretor (BuildingType(..),
                    Command(..))
import Coord
import Data.Bits

buildingTypeToInt :: BuildingType -> Int
buildingTypeToInt DEFENSE = 0
buildingTypeToInt ATTACK  = 1
buildingTypeToInt ENERGY  = 2
buildingTypeToInt TESLA   = 3

intToBuildingType :: Int -> BuildingType
intToBuildingType 0 = DEFENSE
intToBuildingType 1 = ATTACK
intToBuildingType 2 = ENERGY
intToBuildingType 3 = TESLA

type EfficientCommand = Int

nothingCommand :: EfficientCommand
nothingCommand = -1

type EfficientBuilding = Int

build :: Coord -> EfficientBuilding -> EfficientCommand
build bCoord buildingType' = buildingType' .|. (bCoord `shiftL` 2)

toCommand :: EfficientCommand -> Command
toCommand (-1) = NothingCommand
toCommand x    =
  let buildingType = buildingTypeOfCommand x
      coord'       = coordOfCommand x
  in Build coord' buildingType

buildingTypeOfCommand :: EfficientCommand -> BuildingType
buildingTypeOfCommand command = intToBuildingType (command .&. 3)

coordOfCommand :: EfficientCommand -> Int
coordOfCommand x = x `shiftR` 2

-- TODO: handle deconstruct
toEfficientCommand :: Command -> EfficientCommand
toEfficientCommand (Deconstruct _)    = -5
toEfficientCommand (Build coord' bt') = build coord' (buildingTypeToInt bt')
toEfficientCommand NothingCommand     = nothingCommand
