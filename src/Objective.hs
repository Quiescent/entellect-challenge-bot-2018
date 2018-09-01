{-# LANGUAGE BangPatterns #-}

module Objective (oponentsIntermediateBoardScore,
                  myIntermediateBoardScore,
                  playToEnd,
                  confidence,
                  chooseBestMove,
                  Move(..))
  where

import Engine
import Interpretor (Command, GameState(..), Player(..))
import EfficientCommand
import GameState
import AvailableMoves
import VectorIndex
import qualified GameTree    as M
import qualified Data.IntMap as IM

import Data.Maybe
import System.Random
import qualified Data.Vector.Unboxed as UV

import Control.DeepSeq

data Move = Move { myMove       :: Command,
                   oponentsMove :: Command }
          deriving (Show, Eq)

instance NFData Move where
  rnf (Move myMove' oponentsMove') =
    (rnf myMove')       `seq`
    (rnf oponentsMove') `seq`
    ()

oponentsIntermediateBoardScore :: GameState -> Float
oponentsIntermediateBoardScore _ = 1

myIntermediateBoardScore :: GameState -> Float
myIntermediateBoardScore _ = 1

depth :: Int
depth = 100

-- TODO: Check the random generators
playToEnd :: StdGen -> GameState -> M.GameTree -> M.GameTree
playToEnd g initialState gameTree =
  playToEndIter depth g [] initialState gameTree
  where
    -- ===Play Through Tree===
    playToEndIter :: Int -> StdGen -> [PackedCommand] -> GameState -> M.GameTree -> M.GameTree
    playToEndIter 0 _ _     _            gameTree' = gameTree'
    playToEndIter n j moves currentState gameTree' =
      let count                                = M.gamesPlayed gameTree
          ourNode                              = M.subTree moves gameTree
          isEmpty                              = isNothing ourNode
          myMoves                              = myAvailableMoves currentState
          oponentsMoves                        = oponentsAvailableMoves currentState
          oponentsScores                       = M.oponentsScores $ fromJust ourNode
          myScores                             = M.myScores $ fromJust ourNode
          eitherIsUninitialised                = UV.any hasEmptyScore myScores || UV.any hasEmptyScore oponentsScores
          (indexOfMyMove, myMove')             = chooseBestMove' count myMoves       myScores
          (indexOfOponentsMove, oponentsMove') = chooseBestMove' count oponentsMoves oponentsScores
          nextState                            = makeMoves myMove' oponentsMove' currentState
          moves'                               = (combineCommands indexOfMyMove indexOfOponentsMove):moves
      in if gameOver currentState
         then updateEnding currentState moves gameTree'
         else if isEmpty
              then rollout n j currentState moves gameTree'
              else if eitherIsUninitialised
                   then rolloutInitialised n j currentState moves gameTree'
                   else playToEndIter (n - 1) j moves' nextState gameTree'
    -- ===Play Rollout With Initialised==
    rolloutInitialised :: Int -> StdGen -> GameState -> [PackedCommand] -> M.GameTree -> M.GameTree
    rolloutInitialised 0 _ _            _     gameTree' = gameTree'
    rolloutInitialised n k currentState moves gameTree' =
      let oponentsMoves  = oponentsAvailableMoves currentState
          (x, k')        = next k
          oponentIdx     = mod x (UV.length oponentsMoves)
          oponentsMove'  = oponentsMoves `uVectorIndex` oponentIdx
          myMoves        = myAvailableMoves currentState
          (y, k'')       = next k'
          myIdx          = mod y (UV.length myMoves)
          myMove'        = myMoves `uVectorIndex` myIdx
          moves'         = (combineCommands myIdx oponentIdx):moves
          nextState      = makeMoves myMove' oponentsMove' currentState
      in playToEndRandomly (n - 1) k'' moves' nextState gameTree'
    -- ===Initialise Rollout===
    rollout :: Int -> StdGen -> GameState -> [PackedCommand] -> M.GameTree -> M.GameTree
    rollout 0 _ _            _     gameTree' = gameTree'
    rollout n l currentState moves gameTree' =
      let oponentsMoves  = oponentsAvailableMoves currentState
          (x, l')        = next l
          oponentIdx     = mod x (UV.length oponentsMoves)
          oponentsMove'  = oponentsMoves `uVectorIndex` oponentIdx
          myMoves        = myAvailableMoves currentState
          (y, l'')       = next l'
          myIdx          = mod y (UV.length myMoves)
          myMove'        = myMoves `uVectorIndex` myIdx
          myScores       = UV.replicate (UV.length myMoves)       (0, 0)
          oponentsScores = UV.replicate (UV.length oponentsMoves) (0, 0)
          gameTree''     = M.addAt moves (M.GameTree 1 myScores IM.empty oponentsScores) gameTree'
          moves'         = (combineCommands myIdx oponentIdx):moves
          nextState      = makeMoves myMove' oponentsMove' currentState
      in playToEndRandomly (n - 1) l'' moves' nextState gameTree''
    -- ===Random Playout===
    playToEndRandomly :: Int -> StdGen -> [PackedCommand] -> GameState -> M.GameTree -> M.GameTree
    playToEndRandomly 0 _ _     _            gameTree' = gameTree'
    playToEndRandomly n l moves currentState gameTree' =
      if gameOver currentState
      then updateEnding currentState moves gameTree'
      else let oponentsMoves = oponentsRandomMoves currentState
               (x, l')       = next l
               oponentIdx    = mod x (UV.length oponentsMoves)
               oponentsMove' = oponentsMoves `uVectorIndex` oponentIdx
               myMoves       = myRandomMoves currentState
               (y, l'')      = next l'
               myIdx         = mod y (UV.length myMoves)
               myMove'       = myMoves `uVectorIndex` myIdx
               nextState     = makeMoves myMove' oponentsMove' currentState
      in playToEndRandomly (n - 1) l'' moves nextState gameTree'

hasEmptyScore :: (Int, Int) -> Bool
hasEmptyScore (0, 0) = True
hasEmptyScore _      = False

updateEnding :: GameState -> [PackedCommand] -> M.GameTree -> M.GameTree
updateEnding currentState moves gameTree' =
  if iWon currentState
  then updateWin  moves gameTree'
  else updateLoss moves gameTree'

-- Moves are added to an accumulater by consing onto the front; thus,
-- they are reversed when we arrive here.
updateWin :: [PackedCommand] -> M.GameTree -> M.GameTree
updateWin moves gameTree =
  incrementTreeFitness (reverse moves) gameTree

incrementTreeFitness :: [PackedCommand] -> M.GameTree -> M.GameTree
incrementTreeFitness moves gameTree =
  M.incrementDecrement moves gameTree

updateLoss :: [PackedCommand] -> M.GameTree -> M.GameTree
updateLoss moves gameTree =
  decrementTreeFitness (reverse moves) gameTree

decrementTreeFitness :: [PackedCommand] -> M.GameTree -> M.GameTree
decrementTreeFitness moves gameTree =
  M.decrementIncrement moves gameTree

gameOver :: GameState -> Bool
gameOver (GameState { me      = (Player { health = myHealth' }),
                      oponent = (Player { health = oponentsHealth' }) }) =
  (myHealth' <= 0 || oponentsHealth' <= 0)

iWon :: GameState -> Bool
iWon (GameState { me      = (Player { health = myHealth' }),
                  oponent = (Player { health = oponentsHealth' }) }) =
  oponentsHealth' <= 0 && myHealth' > 0

confidence :: Int -> (Int, Int) -> Float
confidence count (wins, games) =
  (w_i / n_i) +
  c * sqrt ((log count_i) / n_i)
  where
    count_i = fromIntegral count
    n_i     = fromIntegral games
    w_i     = fromIntegral wins
    c       = sqrt 2

chooseBestMove' :: Int -> Moves -> UV.Vector (Int, Int) -> (Int, EfficientCommand)
chooseBestMove' matchCount moves scores =
  let indexOfMax = UV.maxIndex $
        UV.map (confidence (fromIntegral matchCount)) scores
  in (indexOfMax, moves `uVectorIndex` indexOfMax)

confidenceDownVotingUninitialised :: Int -> (Int, Int) -> Float
confidenceDownVotingUninitialised _     (_, 0) = -1
confidenceDownVotingUninitialised count scores = confidence count scores

chooseBestMove :: Int -> Moves -> UV.Vector (Int, Int) -> (Int, EfficientCommand)
chooseBestMove matchCount moves scores =
  let indexOfMax = UV.maxIndex $
        UV.map (confidenceDownVotingUninitialised matchCount) scores
  in (indexOfMax, moves `uVectorIndex` indexOfMax)

makeMoves :: EfficientCommand -> EfficientCommand -> GameState -> GameState
makeMoves myMove' oponentsMove' =
  updateMyMove myMove' . updateOponentsMove oponentsMove' . tickEngine
