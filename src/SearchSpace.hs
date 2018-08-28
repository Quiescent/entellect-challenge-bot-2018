{-# LANGUAGE BangPatterns #-}
module SearchSpace (myAvailableMoves,
                    oponentsAvailableMoves,
                    search)
  where

import Interpretor (commandFilePath,
                    GameState(..))
import GameState
import Objective
import EfficientCommand
import VectorIndex
import AvailableMoves
import qualified GameTree as M

import Data.Int
import System.Clock
import System.Random
import Control.Monad.State.Lazy
import qualified Data.Vector.Unboxed as UV
import qualified Data.List           as L

import Control.Concurrent (readChan,
                           newChan,
                           writeChan,
                           forkIO,
                           Chan)

import Control.Parallel.Strategies (using, rdeepseq)
import Control.Exception (evaluate)

maxSearchTime :: Int64
maxSearchTime = 10000000000

timeToNanos :: TimeSpec -> Int64
timeToNanos time = ((sec time) * 1000000000) + nsec time

printCommand :: Command ->  IO ()
printCommand = writeFile commandFilePath . show

-- TODO: implement an initial sleep so that this thread doesn't steel computation time
search :: StdGen -> GameState -> IO ()
search g state' = do
  let clock          = Realtime
  startTime         <- getTime clock
  let startTimeNanos = timeToNanos startTime
  bestMove1          <- newChan
  neighbour1         <- newChan
  bestMove2          <- newChan
  neighbour2         <- newChan
  bestMove3          <- newChan
  neighbour3         <- newChan
  bestMove4          <- newChan
  neighbour4         <- newChan
  _                  <- forkIO $ searchDeeper bestMove1 neighbour2 neighbour4 g state'
  _                  <- forkIO $ searchDeeper bestMove2 neighbour3 neighbour1 g state'
  _                  <- forkIO $ searchDeeper bestMove3 neighbour4 neighbour2 g state'
  _                  <- forkIO $ searchDeeper bestMove4 neighbour1 neighbour3 g state'
  let searchIter = do
        (bestScore1, bestMove1') <- readChan bestMove1
        (bestScore2, bestMove2') <- readChan bestMove2
        (bestScore3, bestMove3') <- readChan bestMove3
        (bestScore4, bestMove4') <- readChan bestMove4
        timeNow                  <- getTime clock
        let best = snd $ L.maximumBy cmpByFst $ [(bestScore1, bestMove1'),
                                                 (bestScore2, bestMove2'),
                                                 (bestScore3, bestMove3'),
                                                 (bestScore4, bestMove4')]
        let timeSoFar = timeToNanos timeNow - startTimeNanos
        putStrLn "Tick"
        printCommand best
        when (timeSoFar < maxSearchTime) $ searchIter
  searchIter

cmpByFst :: (Float, Command) -> (Float, Command) -> Ordering
cmpByFst (x, _) (y, _) = compare x y

ticksBeforeComms :: Int
ticksBeforeComms = 100

commsMidPoint :: Int
commsMidPoint = 50

searchDeeper :: Chan (Float, Command) -> Chan M.GameTree -> Chan M.GameTree -> StdGen -> GameState -> IO ()
searchDeeper best commTo commFrom g initialState =
  searchDeeperIter ticksBeforeComms g M.empty
  where
    searchDeeperIter :: Int -> StdGen -> M.GameTree -> IO ()
    searchDeeperIter !commsCountDown h searchTree =
     let (h', h'')        = split h
         commsCountDown'  = if commsCountDown == 0 then ticksBeforeComms else (commsCountDown - 1)
         searchTree'      = playToEnd h'' initialState searchTree
         scores           = M.myScores searchTree'
         indexOfBestSoFar = UV.maxIndex scores
         scoreOfBestSoFar = scores `uVectorIndex` indexOfBestSoFar
         bestSoFarThunk   = toCommand (moves `uVectorIndex` indexOfBestSoFar)
      in do
        -- putStrLn $ show $ map (\ (score, move) -> "[" ++ show (toCommand move) ++ ": " ++ show score ++ "]") $ zip (UV.toList $ M.myScores searchTree') (UV.toList moves)
        bestSoFar <- evaluate (bestSoFarThunk `using` rdeepseq)
        if (commsCountDown == 0) then do
          writeChan best (scoreOfBestSoFar, bestSoFar)
          neighbourTree <- readChan commFrom
          searchDeeperIter commsCountDown' h' $ M.mergeTrees searchTree' neighbourTree
        else if (commsCountDown == commsMidPoint)
             then do
               writeChan commTo searchTree'
               searchDeeperIter commsCountDown' h' searchTree'
             else searchDeeperIter commsCountDown' h' searchTree'
    moves = myAvailableMoves initialState
