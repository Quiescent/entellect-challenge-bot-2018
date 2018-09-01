module GameTree (GameTree(..),
                 gamesPlayed,
                 mergeTrees,
                 subTree,
                 addAt,
                 empty,
                 hasNoBranches,
                 myScores,
                 isEmpty,
                 oponentsScores,
                 decrementIncrement,
                 incrementDecrement)
  where

import EfficientCommand

import Data.Ratio

import qualified Data.IntMap                 as M
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as MVector

type SubTree = M.IntMap GameTree

type WinLoss = UV.Vector (Float, Float)

data GameTree = GameTree Float WinLoss SubTree WinLoss
              | EmptyTree
  deriving (Show, Eq)

gamesPlayed :: GameTree -> Float
gamesPlayed EmptyTree          = 0
gamesPlayed (GameTree x _ _ _) = x

-- TODO: Don't use nothing (see comment on advance state for way of
-- zipping through tree so that we never need this anyway)
subTree :: [PackedCommand] -> GameTree -> Maybe GameTree
subTree _            EmptyTree                 = Nothing
subTree []           tree                      = Just tree
subTree (move:moves) (GameTree _ _ branches _) = M.lookup move branches >>= subTree moves

incrementDecrement :: [PackedCommand] -> GameTree -> GameTree
incrementDecrement _            EmptyTree = EmptyTree
incrementDecrement []           tree      = tree
incrementDecrement (move:moves) (GameTree count myWinLoss branches oponentsWinLoss) =
  let updatedSubTree = M.adjust (incrementDecrement moves) move branches
  in GameTree count (UV.modify modifyMyScore myWinLoss) updatedSubTree (UV.modify modifyOponentsScore oponentsWinLoss)
  where
    (myMove, oponentsMove) = unpackPackedCommand move
    modifyMyScore scores       = do
      (myWins, myGames) <- MVector.read scores myMove
      MVector.write scores myMove
                    (myWins + 1, myGames + 1)
    modifyOponentsScore scores       = do
      (oponentsWins, oponentsGames) <- MVector.read scores oponentsMove
      MVector.write scores oponentsMove
                    (oponentsWins, oponentsGames + 1)

decrementIncrement :: [PackedCommand] -> GameTree -> GameTree
decrementIncrement _            EmptyTree = EmptyTree
decrementIncrement []           tree      = tree
decrementIncrement (move:moves) (GameTree count myWinLoss branches oponentsWinLoss) =
  let updatedSubTree = M.adjust (decrementIncrement moves) move branches
  in GameTree count (UV.modify modifyMyScore myWinLoss) updatedSubTree (UV.modify modifyOponentsScore oponentsWinLoss)
  where
    (myMove, oponentsMove) = unpackPackedCommand move
    modifyMyScore scores       = do
      (myWins, myGames) <- MVector.read scores myMove
      MVector.write scores myMove
                    (myWins, myGames + 1)
    modifyOponentsScore scores       = do
      (oponentsWins, oponentsGames) <- MVector.read scores oponentsMove
      MVector.write scores oponentsMove
                    (oponentsWins + 1, oponentsGames + 1)

addAt :: [PackedCommand] -> GameTree -> GameTree -> GameTree
addAt _            subTreeToAdd EmptyTree                                                 = subTreeToAdd
addAt []           subTreeToAdd _                                                         = subTreeToAdd
addAt (move:[])    subTreeToAdd (GameTree count myMoveScores branches oponentsMoveScores) =
  let updatedSubTree = M.insert move subTreeToAdd branches
  in GameTree (count + 1) myMoveScores updatedSubTree oponentsMoveScores
addAt (move:moves) subTreeToAdd (GameTree count myMoveScores branches oponentsMoveScores) =
  let updatedSubTree = M.adjust (addAt moves subTreeToAdd) move branches
  in GameTree (count + 1) myMoveScores updatedSubTree oponentsMoveScores

mergeTrees :: GameTree -> GameTree -> GameTree
mergeTrees EmptyTree y                                          = y
mergeTrees x         EmptyTree                                  = x
mergeTrees (GameTree count1 myScores1 myBranches1 enemyScores1)
           (GameTree count2 myScores2 myBranches2 enemyScores2) =
  (GameTree (count1 + count2)
            (UV.zipWith addCorresponding myScores1 myScores2)
            (M.unionWith mergeTrees myBranches1 myBranches2)
            (UV.zipWith addCorresponding enemyScores1 enemyScores2))

addCorresponding :: (Float, Float) -> (Float, Float) -> (Float, Float)
addCorresponding (0, 0) (x', y') = (x', y')
addCorresponding (x, y) (0, 0)   = (x, y)
addCorresponding (x, y) (x', y') =
  let fraction = (toRational x / toRational y) + (toRational x' / toRational y')
  in (fromIntegral $ numerator fraction, fromIntegral $ denominator fraction)

empty :: GameTree
empty = EmptyTree

isEmpty :: GameTree -> Bool
isEmpty EmptyTree = True
isEmpty _         = False

hasNoBranches :: GameTree -> Bool
hasNoBranches EmptyTree        = False
hasNoBranches (GameTree _ _ x _) = M.null x

myScores :: GameTree -> WinLoss
myScores EmptyTree        = UV.empty
myScores (GameTree _ x _ _) = x

oponentsScores :: GameTree -> WinLoss
oponentsScores EmptyTree        = UV.empty
oponentsScores (GameTree _ _ _ x) = x
