module GameMap (mapContents, foldGameMap, mapContentsWithCoords, getAt, adjustAt, definedAt, addAt)
  where

import Interpretor (GameState(..),
                    CellContents(..),
                    SparseMap)
import Data.IntMap as M
import Prelude     as P

-- TODO Don't use this
mapContents :: GameState -> [CellContents]
mapContents state = ((M.elems . gameMap) state) >>= M.elems

-- TODO Don't use this
mapContentsWithCoords :: GameState -> [((Int, Int), CellContents)]
mapContentsWithCoords state =
  (do
     (y, row)  <- rows
     (x, cell) <- M.assocs row
     return ((x, y), cell))
  where
    rows = M.assocs $ gameMap state

adjustAt :: (CellContents -> CellContents) -> (Int, Int) -> SparseMap -> SparseMap
adjustAt f (x, y) = M.adjust (M.adjust f x) y

addAt :: (Int, Int) -> CellContents -> SparseMap -> SparseMap
addAt (x, y) cell = M.adjust (M.insert x cell) y

-- TODO improve the performance of consumers and hopefully drop this
foldGameMap :: ((Int, Int) -> CellContents -> a -> a) -> a -> GameState -> a
foldGameMap f initial = P.foldr f' initial . mapContentsWithCoords
  where f' = \ (coord, cell) z -> f coord cell z

-- TODO make callers not give out of bounds input to this!
getAt :: (Int, Int) -> SparseMap -> Maybe CellContents
getAt (x, y) map' =
  if definedAt (x, y) map'
  then M.lookup x (map' M.! y)
  else Nothing

definedAt :: (Int, Int) -> SparseMap -> Bool
definedAt (x, y) map' =
  M.member y map' && M.member x (map' M.! y)
