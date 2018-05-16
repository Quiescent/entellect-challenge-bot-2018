module GameMap (mapContents, foldGameMap, mapContentsWithCoords)
  where

import Interpretor (GameState(..),
                    CellContents(..))
import Data.Map.Strict as M

mapContents :: GameState -> [CellContents]
mapContents = M.elems . gameMap

mapContentsWithCoords :: GameState -> [((Int, Int), CellContents)]
mapContentsWithCoords = M.assocs . gameMap

foldGameMap :: ((Int, Int) -> CellContents -> a -> a) -> a -> GameState -> a
foldGameMap f initial = M.foldrWithKey f initial . gameMap
