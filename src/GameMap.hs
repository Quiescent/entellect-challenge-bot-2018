module GameMap (mapContents, foldGameMap, mapContentsWithCoords, getAt, adjustAt, definedAt)
  where

import Interpretor (GameState(..),
                    CellContents(..),
                    SparseMap)
import Data.Map.Strict as M

mapContents :: GameState -> [CellContents]
mapContents = M.elems . gameMap

mapContentsWithCoords :: GameState -> [((Int, Int), CellContents)]
mapContentsWithCoords = M.assocs . gameMap

adjustAt :: (CellContents -> CellContents) -> (Int, Int) -> SparseMap -> SparseMap
adjustAt = M.adjust

foldGameMap :: ((Int, Int) -> CellContents -> a -> a) -> a -> GameState -> a
foldGameMap f initial = M.foldrWithKey f initial . gameMap

getAt :: (Int, Int) -> SparseMap -> Maybe CellContents
getAt = M.lookup

definedAt :: (Int, Int) -> SparseMap -> Bool
definedAt = M.member
