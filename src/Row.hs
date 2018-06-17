module Row (rowAt, rowFoldr, rowFoldl')
  where

import Interpretor (Row,
                    CellContents(..))

import Data.IntMap as M

rowAt :: Int -> Row -> Maybe CellContents
rowAt = M.lookup

rowFoldr :: (CellContents -> b -> b) -> b -> Row -> b
rowFoldr = M.foldr

rowFoldl' :: (b -> CellContents -> b) -> b -> Row -> b
rowFoldl' = M.foldl'
