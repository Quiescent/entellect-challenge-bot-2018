module GameTree (GameTree(..),
                 subTree,
                 addAt,
                 empty,
                 hasNoBranches,
                 myScores,
                 oponentsScores,
                 incrementDecrementBy)
  where

import EfficientCommand

import Data.Maybe
import qualified Data.IntMap                 as M
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as MVector

data GameTree = GameTree (UV.Vector Float) (M.IntMap GameTree) (UV.Vector Float)
  deriving (Show, Eq)

subTree :: [PackedCommand] -> GameTree -> Maybe GameTree
subTree []           tree                    = Just tree
subTree (move:moves) (GameTree _ branches _) =
  M.lookup move branches >>= subTree moves

incrementDecrementBy :: [PackedCommand] -> Float -> GameTree -> GameTree
incrementDecrementBy []           x tree = tree
incrementDecrementBy (move:moves) x (GameTree myMoveScores branches oponentsMoveScores) =
  let updatedSubTree          = M.adjust (incrementDecrementBy moves x) move branches
  in GameTree (UV.modify increment myMoveScores) branches (UV.modify decrement oponentsMoveScores)
  where
    (myMove, oponentsMove) = unpackPackedCommand move
    increment scores       = do
      myOldScore <- MVector.read scores myMove
      MVector.write scores myMove (myOldScore + x)
    decrement scores       = do
      oponentsOldScore <- MVector.read scores oponentsMove
      MVector.write scores oponentsMove (oponentsOldScore - x)

addAt :: [PackedCommand] -> GameTree -> GameTree -> GameTree
addAt []           subTreeToAdd _                                                   =
  subTreeToAdd
addAt (move:[])    subTreeToAdd (GameTree myMoveScores branches oponentsMoveScores) =
  let updatedSubTree = M.insert move subTreeToAdd branches
  in GameTree myMoveScores updatedSubTree oponentsMoveScores
addAt (move:moves) subTreeToAdd (GameTree myMoveScores branches oponentsMoveScores) =
  let updatedSubTree = M.adjust (addAt moves subTreeToAdd) move branches
  in GameTree myMoveScores updatedSubTree oponentsMoveScores

empty :: GameTree
empty = GameTree UV.empty M.empty UV.empty

hasNoBranches :: GameTree -> Bool
hasNoBranches (GameTree _ x _) = M.null x

myScores :: GameTree -> UV.Vector Float
myScores (GameTree x _ _) = x

oponentsScores :: GameTree -> UV.Vector Float
oponentsScores (GameTree _ _ x) = x
