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
  _ <- forkIO $ searchDeeper bestMove g state
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

maximumByScore :: [(Float, (GameState, a))] -> (Float, (GameState, a))
maximumByScore = L.maximumBy ( \ (x, _) (y, _) -> compare x y )

-- TODO Remove finished ones from the search as we go
searchDeeper :: RandomGen g => MVar Command -> g -> GameState -> IO ()
searchDeeper best g initialState =
  let (g', selected) = advanceState g (initialState, Move NothingCommand NothingCommand)
  in searchDeeperIter g' $ map myBoardScore selected
  where
    searchDeeperIter :: RandomGen g => g -> [(Float, (GameState, Move))] -> IO ()
    searchDeeperIter g'' states = do
        putMVar best $ rnf bestMoveSoFar `seq` bestMoveSoFar
        selected `pseq` searchDeeperIter g'''' selected
        where
          bestMoveSoFar      = myMove $ snd $ snd $ maximumByScore states
          (g''', nextStates) = scoreNextStates g'' states
          (selected, g'''')  = chooseN breadthToSearch g''' $ zipCDF nextStates

scoreNextStates :: RandomGen g => g -> [(Float, (GameState, Move))] -> (g, [(Float, (GameState, Move))])
scoreNextStates g = foldr scoreNextStatesAcc (g, [])

scoreNextStatesAcc :: RandomGen g => (Float, (GameState, Move)) -> (g, [(Float, (GameState, Move))]) -> (g, [(Float, (GameState, Move))])
scoreNextStatesAcc (_, withMove@(_, move)) (g, statesAcc) =
  rnf threaded `par` (g', scored ++ statesAcc)
  where
    (g', newStates) = advanceState g withMove
    threaded        = map ( \ (state', _) ->  (state', move)) newStates
    scored          = map myBoardScore threaded
  

breadthToSearch :: Int
breadthToSearch = 12

splay :: Int
splay = 5

advanceState :: RandomGen g => g -> (GameState, Move) -> (g, [(GameState, Move)])
advanceState g (gameState, move) =
  if gameOver gameState
  then (g, [(gameState, move)])
  else (g''',
        (do
            myCommand       <- map (snd . snd) myStates
            oponentsCommand <- map (snd . snd) oponentsStates
            return (updateMyMove myCommand $ updateOponentsMove oponentsCommand $ tickEngine gameState,
                    Move myCommand oponentsCommand)))
  where
    chooseCandidates gen = chooseN splay gen . zipCDF
    (g', g'') = split g
    (myStates, _)         =
      chooseCandidates g' $
      map (myBoardScore) $
      myMoves gameState
    (oponentsStates, g''') =
      chooseCandidates g'' $
      invertScores $
      map (myBoardScore) $
      oponentsMoves gameState

myMoves :: GameState -> [(GameState, Command)]
myMoves state = do
  myMove' <- myAvailableMoves state
  return $ (updateMyMove myMove' state, myMove')

oponentsMoves :: GameState -> [(GameState, Command)]
oponentsMoves state = do
  oponentsMove' <- oponentsAvailableMoves state
  return $ (updateOponentsMove oponentsMove' state, oponentsMove')

invertScores :: [(Float, (GameState, a))] -> [(Float, (GameState, a))]
invertScores = map ( \ (score', x) -> (1.0 / score', x))

gameOver :: GameState -> Bool
gameOver (GameState { me      = (Player { health = myHealth }),
                      oponent = (Player { health = oponentsHealth }) }) =
  myHealth == 0 || oponentsHealth == 0

zipCDF :: [(Float, (GameState, a))] -> [(Float, (GameState, a))]
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

chooseN :: (RandomGen g) => Int -> g -> [(Float, (GameState, a))] -> ([(Float, (GameState, a))], g)
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
