{-# LANGUAGE BangPatterns #-}

module Objective (oponentsIntermediateBoardScore,
                  myIntermediateBoardScore,
                  playToEnd,
                  Move(..))
  where

import Engine
import Interpretor (Command, GameState(..), Player(..))
import BitSetMap
import EfficientCommand
import GameState
import AvailableMoves
import VectorIndex
import qualified GameTree    as M
import qualified Data.IntMap as IM

import Data.Maybe
import System.Random
import Control.Monad.State.Lazy
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
depth = 50

type AdvanceStateResult = (StdGen, [PackedCommand], GameState, Bool)

minimumTurnsIntoTheFuture :: Int
minimumTurnsIntoTheFuture = 5

playToEnd :: StdGen -> GameState -> M.GameTree -> M.GameTree
playToEnd g initialState gameTree =
  playToEndIter depth (g, [], initialState, False) gameTree
  where
    startingTurn = gameRound initialState
    -- TODO inline and transfer into secondary loop.
    playToEndIter :: Int -> AdvanceStateResult -> M.GameTree -> M.GameTree
    playToEndIter n advanceStateResult@(_, _, currentState, True) gameTree' =
      if gameOver currentState
      then updateEnding currentState advanceStateResult gameTree'
      else let advanceStateResult' = playRandomly advanceStateResult
           in  playToEndIter (n - 1) advanceStateResult' gameTree'
    -- playToEndIter 0 advanceStateResult                            gameTree' = updateLengthenGameEnding gameTree' advanceStateResult
    playToEndIter 0 _                                             gameTree' = gameTree'
    playToEndIter n advanceStateResult@(_, _, currentState, _)    gameTree' =
      if ((startingTurn - (gameRound currentState) > minimumTurnsIntoTheFuture) && attackTowerGameOver currentState)
      then updateEnding currentState advanceStateResult gameTree'
      else if gameOver currentState
           then updateEnding currentState advanceStateResult gameTree'
           else let (advanceStateResult', gameTree'') = runState (advanceState advanceStateResult) gameTree'
                in  playToEndIter (n - 1) advanceStateResult' gameTree''

genuineWinLossAmplifier :: Float
genuineWinLossAmplifier = 1

updateEnding :: GameState -> AdvanceStateResult -> M.GameTree -> M.GameTree
updateEnding currentState advanceStateResult gameTree' =
  if iWon currentState
  then updateWin  genuineWinLossAmplifier advanceStateResult gameTree'
  else updateLoss genuineWinLossAmplifier advanceStateResult gameTree'

-- Moves are added to an accumulater by consing onto the front; thus,
-- they are reversed when we arrive here.
updateWin :: Float -> AdvanceStateResult -> M.GameTree -> M.GameTree
updateWin winLossAmplifier (_, moves, _, _) gameTree =
  incrementTreeFitness winLossAmplifier (reverse moves) gameTree

incrementTreeFitness :: Float -> [PackedCommand] -> M.GameTree -> M.GameTree
incrementTreeFitness winLossAmplifier moves gameTree =
  M.incrementDecrementBy moves winLossAmplifier gameTree

updateLoss :: Float -> AdvanceStateResult -> M.GameTree -> M.GameTree
updateLoss winLossAmplifier (_, moves, _, _) gameTree =
  decrementTreeFitness winLossAmplifier (reverse moves) gameTree

decrementTreeFitness :: Float -> [PackedCommand] -> M.GameTree -> M.GameTree
decrementTreeFitness winLossAmplifier moves gameTree =
  M.incrementDecrementBy moves (-winLossAmplifier) gameTree

attackDeficit :: GameState -> Int
attackDeficit
  (GameState { me      =
               (Player { attack0Towers                 = myAttack0Towers,
                         attack1Towers                 = myAttack1Towers,
                         attack2Towers                 = myAttack2Towers,
                         attack3Towers                 = myAttack3Towers,
                         attackTowersUnderConstruction = myAttackTowersUnderConstruction }),
               oponent =
               (Player { attack0Towers                 = oponentsAttack0Towers,
                         attack1Towers                 = oponentsAttack1Towers,
                         attack2Towers                 = oponentsAttack2Towers,
                         attack3Towers                 = oponentsAttack3Towers,
                         attackTowersUnderConstruction = oponentsAttackTowersUnderConstruction }) }) =
  (countBuildings myAttack0Towers +
   countBuildings myAttack1Towers +
   countBuildings myAttack2Towers +
   countBuildings myAttack3Towers +
   countBuildings myAttackTowersUnderConstruction) -
  (countBuildings oponentsAttack0Towers +
   countBuildings oponentsAttack1Towers +
   countBuildings oponentsAttack2Towers +
   countBuildings oponentsAttack3Towers +
   countBuildings oponentsAttackTowersUnderConstruction)

attackTowerGameOver :: GameState -> Bool
attackTowerGameOver gameState =
  attackDeficit gameState < -2 ||
  attackDeficit gameState > 2

gameOver :: GameState -> Bool
gameOver (GameState { me      = (Player { health = myHealth' }),
                      oponent = (Player { health = oponentsHealth' }) }) =
  myHealth' <= 0 || oponentsHealth' <= 0

iWon :: GameState -> Bool
iWon (GameState { me      = (Player { health = myHealth' }),
                  oponent = (Player { health = oponentsHealth' }) }) =
  oponentsHealth' <= 0 && myHealth' > 0

-- I'm traversing the tree every time.  This is not necessary!!!
advanceState :: AdvanceStateResult -> State M.GameTree AdvanceStateResult
advanceState (g, moves, currentState, _) = do
  gameTree          <- get
  let ourNode        = M.subTree moves gameTree
  let isEmpty        = isNothing ourNode
  let myMoves        = myAvailableMoves currentState
  let oponentsMoves  = oponentsAvailableMoves currentState
  let (g', oponentsScores) =
        if isEmpty
        then playOponentToEnd g  currentState oponentsMoves
        else (g, M.oponentsScores $ fromJust ourNode)
  let (g'', myScores) =
        if isEmpty
        then playMeToEnd g' currentState myMoves
        else (g', M.myScores $ fromJust ourNode)
  let (indexOfMyMove, myMove')             = chooseBestMove myMoves myScores
  let (indexOfOponentsMove, oponentsMove') = chooseBestMove oponentsMoves oponentsScores
  let nextState = makeMoves myMove' oponentsMove' currentState
  let moves'    =
        if isEmpty
        then moves
        else (combineCommands indexOfMyMove indexOfOponentsMove):moves
  when isEmpty $
    put $ M.addAt moves (M.GameTree myScores IM.empty oponentsScores) gameTree
  return (g'', moves', nextState, isEmpty)

playOponentToEnd :: RandomGen g => g -> GameState -> Moves -> (g, Scores)
playOponentToEnd g _ moves =
  (g, UV.map (\ _ -> 1) moves)

playMeToEnd :: RandomGen g => g -> GameState -> Moves -> (g, Scores)
playMeToEnd g _ moves =
  (g, UV.map (\ _ -> 1) moves)

chooseBestMove :: Moves -> Scores -> (Int, EfficientCommand)
chooseBestMove moves scores =
  let indexOfMax = UV.maxIndex scores
  in (indexOfMax, moves `uVectorIndex` indexOfMax)

makeMoves :: EfficientCommand -> EfficientCommand -> GameState -> GameState
makeMoves myMove' oponentsMove' =
  updateMyMove myMove' . updateOponentsMove oponentsMove' . tickEngine

type Scores = UV.Vector Float

playRandomly :: AdvanceStateResult -> AdvanceStateResult
playRandomly (g, moves, currentState, _) =
  let myMoves               = myAvailableMoves currentState
      oponentsMoves         = oponentsAvailableMoves currentState
      (g', g'')             = split g
      (myMove', _)          = chooseRandomly g' myMoves
      (oponentsMove', g''') = chooseRandomly g'' oponentsMoves
      nextState             = updateMyMove myMove' $
        updateOponentsMove oponentsMove' $
        tickEngine currentState
  in (g''', moves, nextState, True)

chooseRandomly :: (RandomGen g) => g -> Moves -> (EfficientCommand, g)
chooseRandomly g moves =
  (moves `uVectorIndex` idx, g')
  where
    (value, g') = next g
    idx         = mod value (UV.length moves)
