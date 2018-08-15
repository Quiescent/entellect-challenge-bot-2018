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
import qualified GameTree as M

import Data.Int
import System.Clock
import Data.Maybe
import System.Random
import Control.Monad.State.Lazy
import qualified Data.Vector.Unboxed         as UV
import qualified Data.IntMap                 as IM
import qualified Data.Vector.Unboxed.Mutable as MVector
import qualified Data.List                   as L

import Control.Concurrent (killThread,
                           tryTakeMVar,
                           newEmptyMVar,
                           putMVar,
                           forkIO,
                           threadDelay,
                           MVar)

import Control.Parallel.Strategies (using, rdeepseq, parList)
import Control.Exception (evaluate)
import Control.DeepSeq (deepseq)

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

delayTime :: Int
delayTime = 10

-- TODO: implement an initial sleep so that this thread doesn't steel computation time
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

cmpByFst :: (Float, Command) -> (Float, Command) -> Ordering
cmpByFst (x, _) (y, _) = compare x y

type Scores = UV.Vector Float

-- TODO: Start chunking execution
searchDeeper :: MVar Command -> StdGen -> GameState -> IO ()
searchDeeper best g initialState = searchDeeperIter g' treeOne treeTwo treeThree treeFour
  where
    searchDeeperIter :: StdGen -> M.GameTree -> M.GameTree -> M.GameTree -> M.GameTree -> IO ()
    searchDeeperIter h searchTree1 searchTree2 searchTree3 searchTree4 = do
      putStrLn "Tick"
      -- Tree 1
      let (h', h1)          = split h
      let searchTree1'      = playToEnd h1 division1 initialState searchTree1
      let scores1           = M.myScores searchTree1
      let indexOfBestSoFar1 = UV.maxIndex scores1
      let scoreOfBestSoFar1 = scores1 `uVectorIndex` indexOfBestSoFar1
      let bestSoFarThunk1   = toCommand (division1 `uVectorIndex` indexOfBestSoFar1)
      -- Tree 2
      let (h2, h3)          = split h1
      let searchTree2'      = playToEnd h2 division2 initialState searchTree2
      let scores2           = M.myScores searchTree2
      let indexOfBestSoFar2 = UV.maxIndex scores2
      let scoreOfBestSoFar2 = scores2 `uVectorIndex` indexOfBestSoFar2
      let bestSoFarThunk2   = toCommand (division2 `uVectorIndex` indexOfBestSoFar2)
      -- Tree 3
      let (h4, _)           = split h2
      let searchTree3'      = playToEnd h3 division3 initialState searchTree3
      let scores3           = M.myScores searchTree3
      let indexOfBestSoFar3 = UV.maxIndex scores3
      let scoreOfBestSoFar3 = scores3 `uVectorIndex` indexOfBestSoFar3
      let bestSoFarThunk3   = toCommand (division3 `uVectorIndex` indexOfBestSoFar3)
      -- Tree 4
      let searchTree4'      = playToEnd h4 division4 initialState searchTree4
      let scores4           = M.myScores searchTree4
      let indexOfBestSoFar4 = UV.maxIndex scores4
      let scoreOfBestSoFar4 = scores4 `uVectorIndex` indexOfBestSoFar4
      let bestSoFarThunk4   = toCommand (division4 `uVectorIndex` indexOfBestSoFar4)
      -- Aggregate
      let bestSoFarThunk    =
            snd $
            L.maximumBy cmpByFst $
            ([(scoreOfBestSoFar1, bestSoFarThunk1),
               (scoreOfBestSoFar2, bestSoFarThunk2),
               (scoreOfBestSoFar3, bestSoFarThunk3),
               (scoreOfBestSoFar4, bestSoFarThunk4)] `using` parList rdeepseq)
      -- putStrLn $ show $ map (\ (score, move) -> "[" ++ show (toCommand move) ++ ": " ++ show score ++ "]") $ zip (UV.toList $ M.myScores searchTree') (UV.toList moves)
      bestSoFar           <- evaluate (bestSoFarThunk `using` rdeepseq)
      putMVar best $ (bestSoFar `deepseq` bestSoFar)
      searchDeeperIter h' searchTree1' searchTree2' searchTree3' searchTree4'
    moves         = myAvailableMoves initialState
    (g',
     division1,
     division2,
     division3,
     division4)   = fourRandomPartitions g moves
    oponentsMoves = oponentsAvailableMoves initialState
    treeOne       = initialMove initialState division1 oponentsMoves
    treeTwo       = initialMove initialState division2 oponentsMoves
    treeThree     = initialMove initialState division3 oponentsMoves
    treeFour      = initialMove initialState division4 oponentsMoves

depth :: Int
depth = 50

type AdvanceStateResult = (StdGen, [PackedCommand], GameState, Bool)

playToEnd :: StdGen -> Moves -> GameState -> M.GameTree -> M.GameTree
playToEnd g initialMoves initialState gameTree =
  playToEndIter depth (g, [], initialState, False) gameTree
  where
    updateEnding :: GameState -> AdvanceStateResult -> M.GameTree -> M.GameTree
    updateEnding currentState advanceStateResult gameTree' =
      if iWon currentState
      then updateWin  advanceStateResult gameTree'
      else updateLoss advanceStateResult gameTree'
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
      else let (advanceStateResult', gameTree'') = runState (advanceState initialMoves advanceStateResult) gameTree'
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

initialMove :: GameState -> Moves -> Moves -> M.GameTree
initialMove currentState myMoves oponentsMoves =
  let oponentsScores = oponentsNormalisedScores currentState oponentsMoves
      myScores       = myNormalisedScores currentState myMoves
  in M.GameTree myScores IM.empty oponentsScores

advanceState :: Moves -> AdvanceStateResult -> State M.GameTree AdvanceStateResult
advanceState myInitialMoves (g, moves, currentState, _) = do
  gameTree          <- get
  let ourNode        = M.subTree moves gameTree
  let isEmpty        = isNothing ourNode
  let myMoves        = if moves == [] then myInitialMoves else myAvailableMoves currentState
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
  when isEmpty $
    put $ M.addAt moves (M.GameTree myScores IM.empty oponentsScores) gameTree
  return (g''',
          (combineCommands indexOfMyMove indexOfOponentsMove):moves,
          nextState,
          isEmpty)

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

fourRandomPartitions :: RandomGen g => g -> Moves -> (g, Moves, Moves, Moves, Moves)
fourRandomPartitions g moves =
  (g',
   UV.slice start    (start'   - start)       shuffled,
   UV.slice start'   (start''  - start')      shuffled,
   UV.slice start''  (start''' - start'')     shuffled,
   UV.slice start''' (movesLength - start''') shuffled)
  where
    shuffled                 = UV.map (uVectorIndex moves) shuffledIndices
    remaining                = movesLength `mod` 4
    runLength                = movesLength `div` 4
    start                    = 0
    start'                   = runLength             + if remaining >= 1 then 1 else 0
    start''                  = start'    + runLength + if remaining >= 2 then 1 else 0
    start'''                 = start''   + runLength + if remaining >= 3 then 1 else 0
    movesLength              = UV.length moves
    indices                  = UV.generate movesLength id
    (g', shuffledIndices)    = shuffleIter g (movesLength - 1) indices
    shuffleIter :: RandomGen g => g -> Int -> UV.Vector Int -> (g, UV.Vector Int)
    shuffleIter g'' 0 indices' = (g'', indices')
    shuffleIter g'' n indices' =
      let (value, g''') = next g''
          swapIdx      = value `mod` n
          swap xs      = do
            swapValue1 <- MVector.read xs swapIdx
            swapValue2 <- MVector.read xs       n
            MVector.write xs swapIdx swapValue2
            MVector.write xs n       swapValue1
      in shuffleIter g''' (n - 1) (UV.modify swap indices')
