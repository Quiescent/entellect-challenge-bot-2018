module EfficientCommand where

import Interpretor (BuildingType(..),
                    Command(..))
import Coord
import Data.Bits

buildingTypeToInt :: BuildingType -> Int
buildingTypeToInt DEFENSE = 1
buildingTypeToInt ATTACK  = 2
buildingTypeToInt ENERGY  = 3
buildingTypeToInt TESLA   = 4

intToBuildingType :: Int -> BuildingType
intToBuildingType 1 = DEFENSE
intToBuildingType 2 = ATTACK
intToBuildingType 3 = ENERGY
intToBuildingType 4 = TESLA

type EfficientCommand = Int

nothingCommand :: EfficientCommand
nothingCommand = 0

type EfficientBuilding = Int

build :: Coord -> EfficientBuilding -> EfficientCommand
build bCoord buildingType' = buildingType' .|. (bCoord `shiftL` 3)

toCommand :: EfficientCommand -> Command
toCommand 0 = NothingCommand
toCommand x =
  let buildingType = buildingTypeOfCommand x
      coord'       = coordOfCommand x
  in Build coord' buildingType

buildingTypeOfCommand :: EfficientCommand -> BuildingType
buildingTypeOfCommand command = intToBuildingType (command .&. 7)

coordOfCommand :: EfficientCommand -> Int
coordOfCommand x = x `shiftR` 3

-- TODO: handle deconstruct
toEfficientCommand :: Command -> EfficientCommand
toEfficientCommand (Deconstruct _)    = -5
toEfficientCommand (Build coord' bt') = build coord' (buildingTypeToInt bt')
toEfficientCommand NothingCommand     = nothingCommand

type PackedCommand = Int

-- This is actually being used to pack indices of moves in the search
-- tree.  The name is misleading and if they add a lot more towers
-- then I'll have to accomodate for it here.
combineCommands :: EfficientCommand -> EfficientCommand -> PackedCommand
combineCommands x y = x .|. (y `shiftL` 9)

unpackPackedCommand :: PackedCommand -> (EfficientCommand, EfficientCommand)
unpackPackedCommand x =
  (x .&. 511, (x `shiftR` 9) .&. 511)