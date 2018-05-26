module GameState (update)
  where

import Interpretor (GameState(..),
                    CellContents(..),
                    BuildingType(..),
                    Command(..))
import GameMap

-- TODO implement executing a command
update :: GameState -> Command -> GameState
update state NothingCommand                                                   = state
update state@(GameState { gameMap = gameMap' }) (Command x' y' buildingType') =
  case (getAt (x', y') gameMap') of
    Nothing                          -> state
    (Just (CellContents (Just _) _)) -> state
    (Just (CellContents Nothing  _)) ->
      state { gameMap = adjustAt (addBuilding buildingType')
                                 (x', y')
                                 gameMap' }

todo = undefined

addBuilding :: BuildingType -> CellContents -> CellContents
addBuilding ATTACK  contents = todo
addBuilding DEFENSE contents = todo
addBuilding ENERGY  contents = todo
