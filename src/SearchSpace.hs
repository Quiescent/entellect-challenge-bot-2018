{-# LANGUAGE BangPatterns #-}

module SearchSpace (advanceState,
                    myAvailableMoves,
                    oponentsAvailableMoves,
                    search)
  where

import Interpretor (GameState(..),
                    Player(..),
                    BuildingType(..))
import Player
import Engine
import Cell
import GameState
import Objective
import Coord
import Magic
import EfficientCommand
import VectorIndex
import qualified GameTree    as M
import qualified Data.IntMap as IM

import Data.Int
import System.Clock
import Data.Maybe
import System.Random
import Control.Monad.State.Lazy
import qualified Data.Vector.Unboxed as UV

import Control.Concurrent (killThread,
                           tryTakeMVar,
                           newEmptyMVar,
                           putMVar,
                           forkIO,
                           threadDelay,
                           MVar)

import Control.Parallel.Strategies (using, rdeepseq)
import Control.Exception (evaluate)
import Control.DeepSeq (deepseq)

xPredicate :: (Int -> Bool) -> Coord -> Bool
xPredicate p coord =
  let x = getX coord
  in p x

type Cells = UV.Vector Coord

myCells :: Cells
myCells = allCells

-- myFrontCells :: Cells
-- myFrontCells = UV.filter (xPredicate (== 7)) myCells

myForwardCells :: Cells
myForwardCells = UV.filter (xPredicate (>= 6)) myCells

-- myMidToFrontCells :: Cells
-- myMidToFrontCells = UV.filter (xPredicate (>= 2)) myCells

-- myBackCells :: Cells
-- myBackCells = UV.filter (xPredicate (<= 1)) myCells

oponentsCells :: Cells
oponentsCells = allCells

-- oponentsFrontCells :: Cells
-- oponentsFrontCells = UV.filter (xPredicate (== 8)) oponentsCells

oponentsForwardCells :: Cells
oponentsForwardCells = UV.filter (xPredicate (<= 9)) $ oponentsCells

-- oponentsMidToFrontCells :: Cells
-- oponentsMidToFrontCells = UV.filter (xPredicate (<= 13)) oponentsCells

-- oponentsBackCells :: Cells
-- oponentsBackCells = UV.filter (xPredicate (>= 14)) oponentsCells

type Moves = UV.Vector EfficientCommand

addNothingCommand :: Moves -> Moves
addNothingCommand = UV.cons nothingCommand

allMovesOfType :: BuildingType -> Cells -> Moves
allMovesOfType buildingType' cells' =
  UV.map ((flip build) (buildingTypeToInt buildingType')) cells'

-- allMyBackEnergyTowerMoves :: Moves
-- allMyBackEnergyTowerMoves = allMovesOfType ENERGY myBackCells

allMyEnergyTowerMoves :: Moves
allMyEnergyTowerMoves = allMovesOfType ENERGY myCells

allMyDefenseTowerMoves :: Moves
allMyDefenseTowerMoves = allMovesOfType DEFENSE myCells

allMyAttackTowerMoves :: Moves
allMyAttackTowerMoves = allMovesOfType ATTACK myCells

allMyFrontTeslaTowerMoves ::  Moves
allMyFrontTeslaTowerMoves = allMovesOfType TESLA myForwardCells

-- allOponentsBackEnergyTowerMoves :: Moves
-- allOponentsBackEnergyTowerMoves = allMovesOfType ENERGY oponentsBackCells

allOponentsEnergyTowerMoves :: Moves
allOponentsEnergyTowerMoves = allMovesOfType ENERGY oponentsCells

allOponentsDefenseTowerMoves :: Moves
allOponentsDefenseTowerMoves = allMovesOfType DEFENSE oponentsCells

allOponentsAttackTowerMoves :: Moves
allOponentsAttackTowerMoves = allMovesOfType ATTACK oponentsCells

allOponentsFrontTeslaTowerMoves ::  Moves
allOponentsFrontTeslaTowerMoves = allMovesOfType TESLA oponentsForwardCells

switchAffordableMoves :: Moves -> Moves -> Moves -> Moves -> Int -> Int -> Moves
switchAffordableMoves energyTowerMoves
                      defenseTowerMoves
                      attackTowerMoves
                      teslaTowerMoves
                      energy'
                      energyPerTurn'
  | energy' < energyTowerCost = UV.singleton nothingCommand
  | energy' < attackTowerCost = addNothingCommand $ energyTowerMoves
  | energy' < teslaTowerCost  = addNothingCommand $
    if energyPerTurn' > attackTowerCost
    then attackTowerMoves UV.++ defenseTowerMoves
    else attackTowerMoves UV.++ defenseTowerMoves UV.++ energyTowerMoves
  | otherwise                 = addNothingCommand $
    attackTowerMoves UV.++
    defenseTowerMoves UV.++
    energyTowerMoves UV.++
    teslaTowerMoves

-- NOTE: Assumes that attack towers cost the same as defense towers
switchMovesICanAfford :: Int -> Int -> Moves
switchMovesICanAfford =
  switchAffordableMoves allMyEnergyTowerMoves
                        allMyDefenseTowerMoves
                        allMyAttackTowerMoves
                        allMyFrontTeslaTowerMoves

myAvailableMoves :: GameState -> Moves
myAvailableMoves (GameState { me = player@(Player { energy = energy' }) }) = do
  UV.filter available affordableMoves
  where
    available 0       = True
    available command = let i = coordOfCommand command in availableCoord i player
    energyGenPerTurn' = energyGenPerTurn player
    affordableMoves   = switchMovesICanAfford energy' energyGenPerTurn'

switchMovesOponentCanAfford :: Int -> Int -> Moves
switchMovesOponentCanAfford =
  switchAffordableMoves allOponentsEnergyTowerMoves
                        allOponentsDefenseTowerMoves
                        allOponentsAttackTowerMoves
                        allOponentsFrontTeslaTowerMoves

oponentsAvailableMoves :: GameState -> Moves
oponentsAvailableMoves (GameState { oponent = player@(Player { energy = energy' }) }) =
  UV.filter available affordableMoves
  where
    available 0       = True
    available command = let i = coordOfCommand command in availableCoord i player
    energyGenPerTurn' = energyGenPerTurn player
    affordableMoves   = switchMovesOponentCanAfford energy' energyGenPerTurn'

maxSearchTime :: Int64
maxSearchTime = 1900000000

timeToNanos :: TimeSpec -> Int64
timeToNanos time = ((sec time) * 1000000000) + nsec time

delayTime :: Int
delayTime = 10

search :: StdGen -> GameState -> IO Command
search g state' = do
  let clock          = Realtime
  startTime         <- getTime clock
  let startTimeNanos = timeToNanos startTime
  bestMove          <- newEmptyMVar
  threadId          <- forkIO $ searchDeeper bestMove g state'
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

searchDeeper :: MVar Command -> StdGen -> GameState -> IO ()
searchDeeper best g initialState = searchDeeperIter g M.empty
  where
    searchDeeperIter :: StdGen -> M.GameTree -> IO ()
    searchDeeperIter h searchTree = do
      putStrLn "Tick"
      let (h', h'')        = split h
      let searchTree'      = playToEnd h' initialState searchTree
      -- putStrLn $ show $ map (\ (score, move) -> "[" ++ show (toCommand move) ++ ": " ++ show score ++ "]") $ zip (UV.toList $ M.myScores searchTree') (UV.toList moves)
      let scores           = M.myScores searchTree'
      let indexOfBestSoFar = UV.maxIndex scores
      let bestSoFarThunk   = toCommand (moves `uVectorIndex` indexOfBestSoFar)
      bestSoFar           <- evaluate (bestSoFarThunk `using` rdeepseq)
      putMVar best $ (bestSoFar `deepseq` bestSoFar)
      searchDeeperIter h'' searchTree'
    moves = myAvailableMoves initialState

depth :: Int
depth = 50

type AdvanceStateResult = (StdGen, [PackedCommand], GameState)

playToEnd :: StdGen -> GameState -> M.GameTree -> M.GameTree
playToEnd g initialState gameTree =
  playToEndIter depth (g, [], initialState) gameTree
  where
    playToEndIter :: Int -> AdvanceStateResult -> M.GameTree -> M.GameTree
    playToEndIter 0 (_, _, _)                               gameTree' = gameTree'
    playToEndIter n advanceStateResult@(_, _, currentState) gameTree' =
      if gameOver currentState
      then if iWon currentState
           then updateWin  advanceStateResult gameTree'
           else updateLoss advanceStateResult gameTree'
      else let (advanceStateResult', gameTree'') = runState (advanceState advanceStateResult) gameTree'
           in  playToEndIter (n - 1) advanceStateResult' gameTree''

-- Moves are added to an accumulater by consing onto the front; thus,
-- they are reversed when we arrive here.
updateWin :: AdvanceStateResult -> M.GameTree -> M.GameTree
updateWin (_, moves, _) gameTree =
  incrementTreeFitness (reverse moves) gameTree

incrementTreeFitness :: [PackedCommand] -> M.GameTree -> M.GameTree
incrementTreeFitness moves gameTree =
  M.incrementDecrementBy moves 1.0 gameTree

updateLoss :: AdvanceStateResult -> M.GameTree -> M.GameTree
updateLoss (_, moves, _) gameTree =
  decrementTreeFitness (reverse moves) gameTree

decrementTreeFitness :: [PackedCommand] -> M.GameTree -> M.GameTree
decrementTreeFitness moves gameTree =
  M.incrementDecrementBy moves (-1.0) gameTree

gameOver :: GameState -> Bool
gameOver (GameState { me      = (Player { health = myHealth' }),
                      oponent = (Player { health = oponentsHealth' }) }) =
  myHealth' <= 0 || oponentsHealth' <= 0

iWon :: GameState -> Bool
iWon (GameState { me      = (Player { health = myHealth' }),
                  oponent = (Player { health = oponentsHealth' }) }) =
  oponentsHealth' <= 0 && myHealth' > 0

advanceState :: AdvanceStateResult -> State M.GameTree AdvanceStateResult
advanceState (g, moves, currentState) = do
  gameTree                  <- get
  let ourNode                = M.subTree moves gameTree
  let isEmpty                = isNothing ourNode
  let myScoresAreEmpty       = isEmpty || (UV.null $ M.myScores       $ fromJust ourNode)
  let oponentsScoresAreEmpty = isEmpty || (UV.null $ M.oponentsScores $ fromJust ourNode)
  let myMoves                = myAvailableMoves currentState
  let numberOfMyMoves        = fromIntegral $ UV.length myMoves
  let oponentsMoves          = oponentsAvailableMoves currentState
  let numberOfOponentMoves   = fromIntegral $ UV.length oponentsMoves
  let oponentsScores         =
        if oponentsScoresAreEmpty
        then UV.map
             ((1.0 /) . (/ numberOfOponentMoves) . myIntermediateBoardScore . (flip updateOponentsMove currentState))
             oponentsMoves
        else M.oponentsScores $ fromJust ourNode
  let myScores =
        if myScoresAreEmpty
        then UV.map
             ((/ numberOfMyMoves) . myIntermediateBoardScore . (flip updateMyMove currentState))
             myMoves
        else M.myScores $ fromJust ourNode
  when (isEmpty || myScoresAreEmpty || oponentsScoresAreEmpty) $
    put $ M.addAt moves (M.GameTree myScores IM.empty oponentsScores) gameTree
  let (g', g'') = split g
  let (indexOfMyMove, myMove', _)         =
        chooseOne g' myMoves $
        cdf $
        myScores
  let (indexOfOponentsMove, oponentsMove', g''') =
        chooseOne g'' oponentsMoves $
        cdf $
        oponentsScores
  let nextState = updateMyMove myMove' $
        updateOponentsMove oponentsMove' $
        tickEngine currentState
  return (g''',
          (combineCommands indexOfMyMove indexOfOponentsMove):moves,
          nextState)

cdf :: UV.Vector Float -> UV.Vector Float
cdf xs = normalised
  where
    normalised = UV.map (/ (UV.head summed)) summed
    summed     = (UV.reverse . UV.scanl1 (+)) adjusted
    adjusted   = UV.map (+minValue) xs
    minValue   = abs $ UV.minimum xs

chooseOne :: (RandomGen g) => g -> Moves -> UV.Vector Float -> (Int, EfficientCommand, g)
chooseOne g moves scores =
  (indexOfMove, scanForValue , g')
  where
    (_, max')     = genRange g
    floatingMax   = fromIntegral max'
    normalise     = (/ floatingMax) . fromIntegral . abs
    (value, g')   = next g
    normalised    = normalise value
    numberOfMoves = UV.length moves
    indexOfLast   = numberOfMoves - 1
    indexOfMove   =
      fromJust $
      lastIfNothing indexOfLast $
      fmap ( \ x -> x - 1) $
      UV.findIndex ((<= normalised)) scores
    scanForValue  = indexMoves indexOfMove
    indexMoves i  = moves `uVectorIndex` (numberOfMoves - i - 1)

lastIfNothing :: Int -> Maybe Int -> Maybe Int
lastIfNothing _         x@(Just _) = x
lastIfNothing lastIndex Nothing    = Just lastIndex
