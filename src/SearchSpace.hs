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

import Control.Concurrent (killThread,
                           tryTakeMVar,
                           newEmptyMVar,
                           putMVar,
                           forkIO,
                           threadDelay,
                           MVar)

import Control.Parallel (pseq)
import Control.Parallel.Strategies (parList, using, rdeepseq)
import Control.DeepSeq (rnf)

availableMoves :: (Coord -> Bool) -> Player -> [Command]
availableMoves constrainCellsTo player@(Player { towerMap = towerMap',
                                                 constructionQueue = constructionQueue' }) = do
  coord         <- filter (not . (flip definedAt) towerMap') $ filter (not . (flip containsSite constructionSites)) $ filter constrainCellsTo allCells
  buildingType' <- buildingsWhichICanAfford
  return $ Build coord buildingType'
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
  threadId          <- forkIO $ searchDeeper bestMove g state
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
          else do
            killThread threadId -- this had no effect :(
            return newBest
  searchIter undefined -- It's not quitting the program here :/

maximumByScore :: [(Float, Command)] -> (Float, Command)
maximumByScore = L.maximumBy ( \ (x, _) (y, _) -> compare x y )

type ItemsUnderSearch = [(Float, Command)]

splitNWays :: RandomGen g => g -> Int -> [g]
splitNWays g 0 = [g]
splitNWays g n = let (g', g'') = split g
                 in g' : splitNWays g'' (n - 1)

searchDeeper :: RandomGen g => MVar Command -> g -> GameState -> IO ()
searchDeeper best g initialState = searchDeeperIter g initialItems
  where
    searchDeeperIter :: RandomGen g => g -> ItemsUnderSearch -> IO ()
    searchDeeperIter h items = do
      putStrLn "Tick"
      let h':hs     = splitNWays h (length items)
      let newItems  = zipWith (playOnceToEnd initialState) items hs
      let newItPar  = newItems `using` parList rdeepseq
      let bestSoFar = maximumByScore newItPar
      putStrLn $ "Best so far: " ++ (show bestSoFar)
      putMVar best $ snd $ rnf bestSoFar `pseq` bestSoFar
      searchDeeperIter h' newItems
    initialItems :: ItemsUnderSearch
    initialItems = zip (repeat 0) $ myAvailableMoves initialState

playOnceToEnd :: RandomGen g => GameState -> (Float, Command) -> g -> (Float, Command)
playOnceToEnd initialState (score, firstMove) g =
  (score + endScore, firstMove)
  where
    (_, endScore) = playToEnd g initialState firstMove

depth :: Int
depth = 20

playToEnd :: RandomGen g => g -> GameState -> Command -> (g, Float)
playToEnd g initialState firstMove =
  let (g', initialMoveMade) = initialAdvanceState g firstMove initialState
  in playToEndIter depth g' initialMoveMade
  where
    playToEndIter :: RandomGen g => Int -> g -> GameState -> (g, Float)
    playToEndIter 0 h currentState = (h, fst $ myBoardScore (currentState, undefined))
    playToEndIter n h currentState =
      if gameOver currentState
      then (h, fst $ myBoardScore (currentState, undefined))
      else let (h', newState) = advanceState h currentState
           in playToEndIter (n - 1) h' newState

gameOver :: GameState -> Bool
gameOver (GameState { me      = (Player { health = myHealth }),
                      oponent = (Player { health = oponentsHealth }) }) =
  myHealth == 0 || oponentsHealth == 0

initialAdvanceState :: RandomGen g => g -> Command -> GameState -> (g, GameState)
initialAdvanceState g firstMove gameState =
  let oponentsCommand = snd $ snd oponentsState
  in (g', updateMyMove firstMove $ updateOponentsMove oponentsCommand $ tickEngine gameState)
  where
    chooseCandidate gen = chooseOne gen . zipCDF
    (oponentsState, g') =
      chooseCandidate g $
      invertScores $
      map (myBoardScore) $
      oponentsMoves gameState

advanceState :: RandomGen g => g -> GameState -> (g, GameState)
advanceState g gameState =
  let myCommand       = snd $ snd myState
      oponentsCommand = snd $ snd oponentsState
  in (g''', updateMyMove myCommand $ updateOponentsMove oponentsCommand $ tickEngine gameState)
  where
    chooseCandidate gen = chooseOne gen . zipCDF
    (g', g'') = split g
    (myState, _)         =
      chooseCandidate g' $
      map (myBoardScore) $
      myMoves gameState
    (oponentsState, g''') =
      chooseCandidate g'' $
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

chooseOne :: (RandomGen g) => g -> [(Float, (GameState, Command))] -> ((Float, (GameState, Command)), g)
chooseOne g xs =
  (scanForValue xs, g')
  where
    (_, max')    = genRange g
    floatingMax  = fromIntegral max'
    normalise    = (/ floatingMax) . fromIntegral . abs
    (value, g')  = next g
    normalised   = normalise value
    scanForValue = fromJust . lastIfNothing xs . L.find ((<= normalised) . fst)

lastIfNothing :: [(Float, (GameState, Command))] -> Maybe (Float, (GameState, Command)) -> Maybe (Float, (GameState, Command))
lastIfNothing _  x@(Just _) = x
lastIfNothing xs Nothing    = Just $ last xs
