module GameTree (GameTree(..),
                 subTree,
                 addAt,
                 empty,
                 hasNoBranches,
                 myScores,
                 oponentsScores,
                 setMyScoresAt,
                 setOponentsScoresAt)
  where

import EfficientCommand

import Data.Maybe
import qualified Data.IntMap         as M
import qualified Data.Vector.Unboxed as UV

data GameTree = GameTree (UV.Vector Float) (M.IntMap GameTree) (UV.Vector Float)

subTree :: [EfficientCommand] -> GameTree -> Maybe GameTree
subTree []           tree = Just tree
subTree (move:moves) (GameTree _ branches _) =
  M.lookup move branches >>= subTree moves

setOponentsScoresAt :: [EfficientCommand] -> UV.Vector Float -> GameTree -> GameTree
setOponentsScoresAt []           scores (GameTree myMoveScores branches _) =
  GameTree myMoveScores branches scores
setOponentsScoresAt (move:moves) scores (GameTree myMoveScores branches oponentsMoveScores) =
  let updatedSubTree = M.adjust (setOponentsScoresAt moves scores) move branches
  in GameTree myMoveScores updatedSubTree oponentsMoveScores

setMyScoresAt :: [EfficientCommand] -> UV.Vector Float -> GameTree -> GameTree
setMyScoresAt []           scores (GameTree _ branches oponentsMoveScores) =
  GameTree scores branches oponentsMoveScores
setMyScoresAt (move:moves) scores (GameTree myMoveScores branches oponentsMoveScores) =
  let updatedSubTree = M.adjust (setMyScoresAt moves scores) move branches
  in GameTree myMoveScores updatedSubTree oponentsMoveScores

addAt :: [EfficientCommand] -> GameTree -> GameTree -> GameTree
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
