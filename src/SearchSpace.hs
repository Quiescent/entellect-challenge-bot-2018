{-# LANGUAGE BangPatterns #-}

module SearchSpace (advanceState,
                    myAvailableMoves,
                    oponentsAvailableMoves,
                    search)
  where

import Interpretor (GameState(..),
                    Player(..),
                    Command(..))
import Engine
import GameMap
import Cell
import GameState
import Towers
import Objective
import BuildingsUnderConstruction
import Coord

import Data.Int
import System.Clock
import Data.Maybe
import System.Random
import qualified Data.List as L

import Control.Concurrent (tryTakeMVar,
                           newEmptyMVar,
                           putMVar,
                           forkIO,
                           threadDelay,
                           MVar)

import Control.Parallel (par, pseq)
import Control.DeepSeq (rnf)

availableMoves :: (Coord -> Bool) -> Player -> [Command]
availableMoves constrainCellsTo player@(Player { towerMap = towerMap',
                                                 constructionQueue = constructionQueue' }) = do
  coord         <- filter (not . (flip definedAt) towerMap') $ filter (not . (flip containsSite constructionSites)) $ filter constrainCellsTo allCells
  buildingType' <- buildingsWhichICanAfford
  let (x, y) = fromCoord coord
  return $ Build x y buildingType'
  where
    constructionSites        = buildingConstructionSites constructionQueue'
    buildingsWhichICanAfford = map snd $ filter ((<= energy') . fst) prices
    energy'                  = energy player
    prices                   = towerPrices

myAvailableMoves :: GameState -> [Command]
myAvailableMoves state =
  NothingCommand : (availableMoves cellBelongsToMe $ me state)

oponentsAvailableMoves :: GameState -> [Command]
oponentsAvailableMoves state =
  NothingCommand : (availableMoves cellBelongsToOponent $ oponent state)

maxSearchTime :: Int64
maxSearchTime = 1800000000

timeToNanos :: TimeSpec -> Int64
timeToNanos time = ((sec time) * 1000000000) + nsec time

delayTime :: Int
delayTime = 10

search :: RandomGen g => g -> GameState -> IO Command
search g state = do
  let clock          = Realtime
  startTime         <- getTime clock
  let startTimeNanos = timeToNanos startTime
  bestMove          <- newEmptyMVar
  _ <- forkIO $ searchDeeper bestMove g' selected
  let searchIter bestSoFar = do
        bestMoveSoFar <- tryTakeMVar bestMove
        timeNow       <- getTime clock
        let newBest = if isJust bestMoveSoFar
                      then fromJust bestMoveSoFar
                      else bestSoFar
        let timeSoFar  = timeToNanos timeNow - startTimeNanos
        if timeSoFar < maxSearchTime
          then do
            threadDelay delayTime
            searchIter newBest
          else return newBest
  searchIter undefined
  where
    (selected, g') = chooseN breadthToSearch g $ zipCDF $ map (myBoardScore) initialChoices
    initialChoices = advanceState state

maximumByScore :: [(Float, (GameState, Move))] -> (Float, (GameState, Move))
maximumByScore = L.maximumBy ( \ (x, _) (y, _) -> compare x y )

-- TODO Remove finished ones from the search as we go
searchDeeper :: RandomGen g => MVar Command -> g -> [(Float, (GameState, Move))] -> IO ()
searchDeeper best g states = do
  putStrLn "Tick"
  putMVar best $ rnf bestMoveSoFar `seq` bestMoveSoFar
  selected `pseq` searchDeeper best g' selected
  where
    bestMoveSoFar  = myMove $ snd $ snd $ maximumByScore states
    nextStates     = scoreNextStates states
    (selected, g') = chooseN breadthToSearch g $ zipCDF nextStates

scoreNextStates :: [(Float, (GameState, Move))] -> [(Float, (GameState, Move))]
scoreNextStates = foldr scoreNextStatesAcc []

scoreNextStatesAcc :: (Float, (GameState, Move)) -> [(Float, (GameState, Move))] -> [(Float, (GameState, Move))]
scoreNextStatesAcc (_, (state, move)) statesAcc =
  let newStates = advanceState state
      threaded  = map ( \ (state', _) ->  (state', move)) newStates
      scored    = map myBoardScore threaded
  in rnf scored `par` scored ++ statesAcc

breadthToSearch :: Int
breadthToSearch = 5

advanceState :: GameState -> [(GameState, Move)]
advanceState gameState = do
  myCommand       <- myAvailableMoves       gameState
  oponentsCommand <- oponentsAvailableMoves gameState
  return (updateMyMove myCommand $ updateOponentsMove oponentsCommand $ tickEngine gameState,
           Move myCommand oponentsCommand)

zipCDF :: [(Float, (GameState, Move))] -> [(Float, (GameState, Move))]
zipCDF xs =
  zipWith ( \ x (_, y) -> (x, y)) normalised (head descending : descending)
  where
    descending = reverse sorted
    normalised = map (/ (head summed)) summed
    summed     = (reverse . scanl (+) 0 . map fst) sorted
    sorted     = L.sortOn fst adjusted
    adjusted   = map (\ (boardScore, x) -> (minValue + boardScore, x)) xs
    minValue   = abs $ minimum $ map fst xs

eliteChoices :: Int
eliteChoices = 1

chooseN :: (RandomGen g) => Int -> g -> [(Float, (GameState, Move))] -> ([(Float, (GameState, Move))], g)
chooseN n g xs =
  (elite ++ randomChoices, g''')
  where
    (_, max')              = genRange g
    floatingMax            = fromIntegral max'
    normalise              = (/ floatingMax) . fromIntegral . abs
    elite                  = take eliteChoices xs
    (randomChoices, g''')  = foldr choose ([], g) [1..(n - eliteChoices)]
    choose _ (choices, g') =
      let (value, g'') = next g'
          normalised   = normalise value
          scanForValue = fromJust . lastIfNothing xs . L.find ((<= normalised) . fst)
      in (scanForValue xs : choices, g'')

lastIfNothing :: [(Float, (GameState, a))] -> Maybe (Float, (GameState, a)) -> Maybe (Float, (GameState, a))
lastIfNothing _  x@(Just _) = x
lastIfNothing xs Nothing    = Just $ last xs
