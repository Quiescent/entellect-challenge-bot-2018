module Row (rowAt, rowFoldr, rowFoldl', rowFoldrIndexed)
  where

import Interpretor (Row,
                    Building(..))

import Data.IntMap as M

rowAt :: Int -> Row -> Maybe Building
rowAt = M.lookup

rowFoldr :: (Building -> b -> b) -> b -> Row -> b
rowFoldr = M.foldr

rowFoldrIndexed :: (Int -> Building -> b -> b) -> b -> Row -> b
rowFoldrIndexed = M.foldrWithKey

rowFoldl' :: (b -> Building -> b) -> b -> Row -> b
rowFoldl' = M.foldl'
