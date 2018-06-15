module Row (rowAt, rowFoldr)
  where

import Interpretor (Row,
                    CellContents(..))

import Data.IntMap as M

rowAt :: Int -> Row -> Maybe CellContents
rowAt = M.lookup

rowFoldr :: (CellContents -> b -> b) -> b -> Row -> b
rowFoldr = M.foldr
