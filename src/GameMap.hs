module GameMap (mapContents, foldGameMap)
  where

import Interpretor (GameState(..),
                    CellContents(..))
import Data.Map.Strict as M

mapContents :: GameState -> [CellContents]
mapContents = M.elems . gameMap

foldGameMap :: ((Int, Int) -> CellContents -> a -> a) -> a -> GameState -> a
foldGameMap f initial = M.foldrWithKey f initial . gameMap
