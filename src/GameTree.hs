module GameTree (GameTree(..),
                 subTree,
                 addAt,
                 empty,
                 hasNoBranches,
                 myScores,
                 isEmpty,
                 oponentsScores,
                 incrementDecrementBy)
  where

import EfficientCommand

import qualified Data.IntMap                 as M
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as MVector

data GameTree = GameTree (UV.Vector Float) (M.IntMap GameTree) (UV.Vector Float)
              | EmptyTree
  deriving (Show, Eq)

-- TODO: Don't use nothing (see comment on advance state for way of
-- zipping through tree so that we never need this anyway)
subTree :: [PackedCommand] -> GameTree -> Maybe GameTree
subTree _            EmptyTree               = Nothing
subTree []           tree                    = Just tree
subTree (move:moves) (GameTree _ branches _) = M.lookup move branches >>= subTree moves 

incrementDecrementBy :: [PackedCommand] -> Float -> GameTree -> GameTree
incrementDecrementBy _            _ EmptyTree = EmptyTree
incrementDecrementBy []           _ tree      = tree
incrementDecrementBy (move:moves) x (GameTree myMoveScores branches oponentsMoveScores) =
  let updatedSubTree = M.adjust (incrementDecrementBy moves x) move branches
  in GameTree (UV.modify increment myMoveScores) updatedSubTree (UV.modify decrement oponentsMoveScores)
  where
    (myMove, oponentsMove) = unpackPackedCommand move
    increment scores       = do
      myOldScore <- MVector.read scores myMove
      MVector.write scores myMove
                    (myOldScore + (x / (fromIntegral $ UV.length myMoveScores)))
    decrement scores       = do
      oponentsOldScore <- MVector.read scores oponentsMove
      MVector.write scores oponentsMove
                    (oponentsOldScore - (x / (fromIntegral $ UV.length oponentsMoveScores)))

addAt :: [PackedCommand] -> GameTree -> GameTree -> GameTree
addAt _            subTreeToAdd EmptyTree                                           = subTreeToAdd
addAt []           subTreeToAdd _                                                   = subTreeToAdd
addAt (move:[])    subTreeToAdd (GameTree myMoveScores branches oponentsMoveScores) =
  let updatedSubTree = M.insert move subTreeToAdd branches
  in GameTree myMoveScores updatedSubTree oponentsMoveScores
addAt (move:moves) subTreeToAdd (GameTree myMoveScores branches oponentsMoveScores) =
  let updatedSubTree = M.adjust (addAt moves subTreeToAdd) move branches
  in GameTree myMoveScores updatedSubTree oponentsMoveScores

empty :: GameTree
empty = EmptyTree

isEmpty :: GameTree -> Bool
isEmpty EmptyTree = True
isEmpty _         = False

hasNoBranches :: GameTree -> Bool
hasNoBranches EmptyTree        = False
hasNoBranches (GameTree _ x _) = M.null x

myScores :: GameTree -> UV.Vector Float
myScores EmptyTree        = UV.empty
myScores (GameTree x _ _) = x

oponentsScores :: GameTree -> UV.Vector Float
oponentsScores EmptyTree        = UV.empty
oponentsScores (GameTree _ _ x) = x
