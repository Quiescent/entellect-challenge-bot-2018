{-# LANGUAGE BangPatterns #-}

-- TODO fix the game tree so that it can also be empty...

module SearchSpace (advanceState,
                    myAvailableMoves,
                    oponentsAvailableMoves,
                    search)
  where

import Interpretor (commandFilePath,
                    GameState(..),
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
import qualified GameTree as M

import Data.Int
import Data.Maybe
import System.Clock
import System.Random
import Control.Monad.State.Lazy
import qualified Data.Vector.Unboxed as UV
import qualified Data.IntMap         as IM
import qualified Data.List           as L

import Control.Concurrent (readChan,
                           newChan,
                           writeChan,
                           forkIO,
                           Chan)

import Control.Parallel.Strategies (using, rdeepseq)
import Control.Exception (evaluate)

xPredicate :: (Int -> Bool) -> Coord -> Bool
xPredicate p coord =
  let x = getX coord
  in p x

type Cells = UV.Vector Coord

cells :: Cells
cells = allCells

frontCells :: Cells
frontCells = UV.filter (xPredicate (== 7)) cells

forwardCells :: Cells
forwardCells = UV.filter (xPredicate (>= 6)) cells

midToFrontCells :: Cells
midToFrontCells = UV.filter (xPredicate (>= 2)) cells

backCells :: Cells
backCells = UV.filter (xPredicate (== 0)) cells

twoBackCells :: Cells
twoBackCells = UV.filter (xPredicate (<= 1)) cells

type Moves = UV.Vector EfficientCommand

addNothingCommand :: Moves -> Moves
addNothingCommand = UV.cons nothingCommand

allMovesOfType :: BuildingType -> Cells -> Moves
allMovesOfType buildingType' cells' =
  UV.map ((flip build) (buildingTypeToInt buildingType')) cells'

allBackEnergyTowerMoves :: Moves
allBackEnergyTowerMoves = allMovesOfType ENERGY backCells

-- allEnergyTowerMoves :: Moves
-- allEnergyTowerMoves = allMovesOfType ENERGY cells

-- allDefenseTowerMoves :: Moves
-- allDefenseTowerMoves = allMovesOfType DEFENSE cells

-- allAttackTowerMoves :: Moves
-- allAttackTowerMoves = allMovesOfType ATTACK cells

allForwardDefenseTowerMoves :: Moves
allForwardDefenseTowerMoves = allMovesOfType DEFENSE forwardCells

allMidToFrontAttackTowerMoves :: Moves
allMidToFrontAttackTowerMoves = allMovesOfType ATTACK midToFrontCells

allTwobackEnergyTowerMoves :: Moves
allTwobackEnergyTowerMoves = allMovesOfType ENERGY twoBackCells

allFrontTeslaTowerMoves ::  Moves
allFrontTeslaTowerMoves = allMovesOfType TESLA frontCells

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
  switchAffordableMoves allTwobackEnergyTowerMoves
                        allForwardDefenseTowerMoves
                        allMidToFrontAttackTowerMoves
                        allFrontTeslaTowerMoves

theMagicalRoundWhenIStopMakingEnergyTowers :: Int
theMagicalRoundWhenIStopMakingEnergyTowers = 12

myAvailableMoves :: GameState -> Moves
myAvailableMoves (GameState { gameRound = gameRound',
                              me        = player@(Player { energy = energy' }) }) =
  if gameRound' <= theMagicalRoundWhenIStopMakingEnergyTowers
  then if energy' >= energyTowerCost
       then UV.filter available allBackEnergyTowerMoves
       else UV.singleton nothingCommand
  else UV.filter available affordableMoves
  where
    available 0       = True
    available command = let i = coordOfCommand command in availableCoord i player
    energyGenPerTurn' = energyGenPerTurn player
    affordableMoves   = switchMovesICanAfford energy' energyGenPerTurn'

switchMovesOponentCanAfford :: Int -> Int -> Moves
switchMovesOponentCanAfford =
  switchAffordableMoves allTwobackEnergyTowerMoves
                        allForwardDefenseTowerMoves
                        allMidToFrontAttackTowerMoves
                        allFrontTeslaTowerMoves

oponentsAvailableMoves :: GameState -> Moves
oponentsAvailableMoves (GameState { gameRound = gameRound',
                                    oponent   = player@(Player { energy = energy' }) }) =
  if gameRound' <= theMagicalRoundWhenIStopMakingEnergyTowers
  then if energy' >= energyTowerCost
       then UV.filter available allBackEnergyTowerMoves
       else UV.singleton nothingCommand
  else UV.filter available affordableMoves
  where
    available 0       = True
    available command = let i = coordOfCommand command in availableCoord i player
    energyGenPerTurn' = energyGenPerTurn player
    affordableMoves   = switchMovesOponentCanAfford energy' energyGenPerTurn'

maxSearchTime :: Int64
maxSearchTime = 1900000000

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

type Scores = UV.Vector Float

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


depth :: Int
depth = 50

type AdvanceStateResult = (StdGen, [PackedCommand], GameState, Bool)

playToEnd :: StdGen -> GameState -> M.GameTree -> M.GameTree
playToEnd g initialState gameTree =
  playToEndIter depth (g, [], initialState, False) gameTree
  where
    updateEnding :: GameState -> AdvanceStateResult -> M.GameTree -> M.GameTree
    updateEnding currentState advanceStateResult gameTree' =
      if iWon currentState
      then updateWin  advanceStateResult gameTree'
      else updateLoss advanceStateResult gameTree'
    -- TODO inline and transfer into secondary loop.
    playToEndIter :: Int -> AdvanceStateResult -> M.GameTree -> M.GameTree
    playToEndIter n advanceStateResult@(_, _, currentState, True) gameTree' =
      if gameOver currentState
      then updateEnding currentState advanceStateResult gameTree'
      else let advanceStateResult' = playRandomly advanceStateResult
           in  playToEndIter (n - 1) advanceStateResult' gameTree'
    playToEndIter 0 (_, _, _, _)                                  gameTree' = gameTree'
    playToEndIter n advanceStateResult@(_, _, currentState, _)    gameTree' =
      if gameOver currentState
      then updateEnding currentState advanceStateResult gameTree'
      else let (advanceStateResult', gameTree'') = runState (advanceState advanceStateResult) gameTree'
           in  playToEndIter (n - 1) advanceStateResult' gameTree''

-- Moves are added to an accumulater by consing onto the front; thus,
-- they are reversed when we arrive here.
updateWin :: AdvanceStateResult -> M.GameTree -> M.GameTree
updateWin (_, moves, _, _) gameTree =
  incrementTreeFitness (reverse moves) gameTree

incrementTreeFitness :: [PackedCommand] -> M.GameTree -> M.GameTree
incrementTreeFitness moves gameTree =
  M.incrementDecrementBy moves 1.0 gameTree

updateLoss :: AdvanceStateResult -> M.GameTree -> M.GameTree
updateLoss (_, moves, _, _) gameTree =
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

-- I'm traversing the tree every time.  This is not necessary!!!
advanceState :: AdvanceStateResult -> State M.GameTree AdvanceStateResult
advanceState (g, moves, currentState, _) = do
  gameTree          <- get
  let ourNode        = M.subTree moves gameTree
  let isEmpty        = isNothing ourNode
  let myMoves        = myAvailableMoves currentState
  let oponentsMoves  = oponentsAvailableMoves currentState
  let oponentsScores =
        if isEmpty
        then oponentsNormalisedScores currentState oponentsMoves
        else M.oponentsScores $ fromJust ourNode
  let myScores =
        if isEmpty
        then myNormalisedScores currentState myMoves
        else M.myScores $ fromJust ourNode
  let (g', g'') = split g
  let (indexOfMyMove, myMove', _)                = chooseAMove g'  myMoves myScores
  let (indexOfOponentsMove, oponentsMove', g''') = chooseAMove g'' oponentsMoves oponentsScores
  let nextState = makeMoves myMove' oponentsMove' currentState
  let moves'    =
        if isEmpty
        then moves
        else (combineCommands indexOfMyMove indexOfOponentsMove):moves
  when isEmpty $
    put $ M.addAt moves (M.GameTree myScores IM.empty oponentsScores) gameTree
  return (g''', moves', nextState, isEmpty)

makeMoves :: EfficientCommand -> EfficientCommand -> GameState -> GameState
makeMoves myMove' oponentsMove' =
  updateMyMove myMove' . updateOponentsMove oponentsMove' . tickEngine

chooseAMove :: RandomGen g => g -> Moves -> Scores -> (Int, EfficientCommand, g)
chooseAMove g moves = chooseOne g moves . cdf

myNormalisedScores :: GameState -> Moves -> Scores
myNormalisedScores gameState =
  normaliseScores .
  UV.map
  ((1 + ) . myIntermediateBoardScore . (flip updateMyMove gameState))

oponentsNormalisedScores :: GameState -> Moves -> Scores
oponentsNormalisedScores gameState =
  normaliseScores .
  UV.map
  ((1.0 /) . (1 + ) . myIntermediateBoardScore . (flip updateOponentsMove gameState))

normaliseScores :: Scores -> Scores
normaliseScores xs =
  let maxScore = UV.maximum xs
  in UV.map (/ maxScore) xs

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

cdf :: Scores -> Scores
cdf xs = normalised
  where
    normalised = UV.map (/ (UV.head summed)) summed
    summed     = (UV.reverse . UV.scanl1 (+)) adjusted
    adjusted   = UV.map (+minValue) xs
    minValue   = abs $ UV.minimum xs

chooseOne :: (RandomGen g) => g -> Moves -> Scores -> (Int, EfficientCommand, g)
chooseOne g moves scores =
  (indexOfMove, scanForValue, g')
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
