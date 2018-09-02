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
import AvailableMoves
import qualified GameTree as M

import Data.Maybe
import Data.Int
import System.Clock
import System.Random
-- import qualified Data.Vector.Unboxed as UV

import Control.Exception (evaluate)

import Control.Concurrent (readChan,
                           newChan,
                           writeChan,
                           forkIO,
                           Chan)

maxSearchTime :: Int64
maxSearchTime = 1800000000

timeToNanos :: TimeSpec -> Int64
timeToNanos time = ((sec time) * 1000000000) + nsec time

printCommand :: Command ->  IO ()
printCommand = writeFile commandFilePath . show

search :: StdGen -> GameState -> IO ()
search g state' = do
  let openingMove = openingBookMove state'
      (h, g')  = split g
      (j, g'') = split g'
      (k, l)   = split g''
  if isJust openingMove
    then do
      putStrLn "Playing oponing book move..."
      printCommand $ fromJust openingMove
    else do
      let moves          = myAvailableMoves state'
      let clock          = Realtime
      startTime         <- getTime clock
      let startTimeNanos = timeToNanos startTime
      treeChan1         <- newChan
      treeChan2         <- newChan
      treeChan3         <- newChan
      treeChan4         <- newChan
      _                 <- forkIO $ searchDeeper clock startTimeNanos treeChan1 h state'
      _                 <- forkIO $ searchDeeper clock startTimeNanos treeChan2 j state'
      _                 <- forkIO $ searchDeeper clock startTimeNanos treeChan3 k state'
      _                 <- forkIO $ searchDeeper clock startTimeNanos treeChan4 l state'
      let searchIter = do
            tree1         <- readChan treeChan1
            tree2         <- readChan treeChan2
            tree3         <- readChan treeChan3
            tree4         <- readChan treeChan4
            let finalTree = (M.mergeTrees (M.mergeTrees (M.mergeTrees tree1 tree2) tree3) tree4)
            let scores    = M.myScores finalTree
            let count     = M.gamesPlayed finalTree
            let (_, best) = chooseBestMove count moves scores
            putStrLn "Tick"
            printCommand $ toCommand best
      searchIter

searchDeeper :: Clock -> Int64 -> Chan M.GameTree -> StdGen -> GameState -> IO ()
searchDeeper clock startingTime best g initialState =
  searchDeeperIter g M.empty
  where
    searchDeeperIter :: StdGen -> M.GameTree -> IO ()
    searchDeeperIter h searchTree =
     let (h', h'')       = split h
      in do
        timeNow      <- getTime clock
        searchTree'  <- evaluate $ playToEnd h'' initialState searchTree
        let timeSoFar = timeToNanos timeNow - startingTime
        if timeSoFar > maxSearchTime
        then writeChan best searchTree
        else searchDeeperIter h' searchTree'
