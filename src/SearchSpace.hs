{-# LANGUAGE BangPatterns #-}

module SearchSpace (advanceState,
                    myAvailableMoves,
                    oponentsAvailableMoves,
                    search)
  where

import Interpretor (GameState(..),
                    Player(..),
                    BuildingType(..),
                    Command(..))
import Engine
import GameMap
import Cell
import GameState
import Objective
import BuildingsUnderConstruction
import Coord
import Magic
import EfficientCommand

import Data.Int
import System.Clock
import Data.Maybe
import System.Random
import qualified Data.List           as L
import qualified Data.Vector         as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as UV

import Control.Concurrent (killThread,
                           tryTakeMVar,
                           newEmptyMVar,
                           putMVar,
                           forkIO,
                           threadDelay,
                           MVar)

import Control.Parallel (pseq)
import Control.Parallel.Strategies (parList, using, rdeepseq)
import Control.Exception (evaluate)
import Control.DeepSeq (rnf, deepseq)

import Debug.Trace

data FloatEvaluator = FloatEvaluator !Float

xPredicate :: (Int -> Bool) -> Coord -> Bool
xPredicate p coord =
  let x = getX coord
  in p x

type Cells = UV.Vector Coord

myCells :: Cells
myCells = UV.filter cellBelongsToMe $ allCells

myFrontCells :: Cells
myFrontCells = UV.filter (xPredicate (== 7)) myCells

myForwardCells :: Cells
myForwardCells = UV.filter (xPredicate (>= 6)) myCells

myMidToFrontCells :: Cells
myMidToFrontCells = UV.filter (xPredicate (>= 2)) myCells

myBackCells :: Cells
myBackCells = UV.filter (xPredicate (<= 1)) myCells

oponentsCells :: Cells
oponentsCells = UV.filter cellBelongsToOponent $ allCells

oponentsFrontCells :: Cells
oponentsFrontCells = UV.filter (xPredicate (== 8)) oponentsCells

oponentsForwardCells :: Cells
oponentsForwardCells = UV.filter (xPredicate (<= 9)) $ oponentsCells

oponentsMidToFrontCells :: Cells
oponentsMidToFrontCells = UV.filter (xPredicate (<= 13)) oponentsCells

oponentsBackCells :: Cells
oponentsBackCells = UV.filter (xPredicate (>= 14)) oponentsCells

type Moves = UV.Vector EfficientCommand

addNothingCommand :: Moves -> Moves
addNothingCommand = UV.cons nothingCommand

allMovesOfType :: BuildingType -> Cells -> Moves
allMovesOfType buildingType' cells' =
  UV.map ((flip build) (buildingTypeToInt buildingType')) cells'

allMyBackEnergyTowerMoves :: Moves
allMyBackEnergyTowerMoves = allMovesOfType ENERGY myBackCells

allMyForwardDefenseTowerMoves :: Moves
allMyForwardDefenseTowerMoves = allMovesOfType DEFENSE myForwardCells

allMyMidToFrontAttackTowerMoves :: Moves
allMyMidToFrontAttackTowerMoves = allMovesOfType ATTACK myMidToFrontCells

allMyFrontTeslaTowerMoves ::  Moves
allMyFrontTeslaTowerMoves = allMovesOfType TESLA myFrontCells

allOponentsBackEnergyTowerMoves :: Moves
allOponentsBackEnergyTowerMoves = allMovesOfType ENERGY oponentsBackCells

allOponentsForwardDefenseTowerMoves :: Moves
allOponentsForwardDefenseTowerMoves = allMovesOfType DEFENSE oponentsForwardCells

allOponentsMidToFrontAttackTowerMoves :: Moves
allOponentsMidToFrontAttackTowerMoves = allMovesOfType ATTACK oponentsMidToFrontCells

allOponentsFrontTeslaTowerMoves ::  Moves
allOponentsFrontTeslaTowerMoves = allMovesOfType TESLA oponentsFrontCells

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
  switchAffordableMoves allMyBackEnergyTowerMoves
                        allMyForwardDefenseTowerMoves
                        allMyMidToFrontAttackTowerMoves
                        allMyFrontTeslaTowerMoves

myAvailableMoves :: GameState -> Moves
myAvailableMoves (GameState { me = (Player { towerMap          = towerMap',
                                             energy            = energy',
                                             energyGenPerTurn  = energyGenPerTurn',
                                             constructionQueue = constructionQueue' }) }) = do
  UV.filter available affordableMoves
  where
    available (-1)       = True
    available command    = let i = coordOfCommand command in notUnderConstruction i && notTaken i
    notUnderConstruction = (not . (flip containsSite constructionSites))
    notTaken             = (not . (flip definedAt) towerMap')
    affordableMoves      = switchMovesICanAfford energy' energyGenPerTurn'
    constructionSites    = buildingConstructionSites constructionQueue'

switchMovesOponentCanAfford :: Int -> Int -> Moves
switchMovesOponentCanAfford =
  switchAffordableMoves allOponentsBackEnergyTowerMoves
                        allOponentsForwardDefenseTowerMoves
                        allOponentsMidToFrontAttackTowerMoves
                        allOponentsFrontTeslaTowerMoves

oponentsAvailableMoves :: GameState -> Moves
oponentsAvailableMoves (GameState { me = (Player { towerMap          = towerMap',
                                                   energy            = energy',
                                                   energyGenPerTurn  = energyGenPerTurn',
                                                   constructionQueue = constructionQueue' }) }) =
  UV.filter available affordableMoves
  where
    available (-1)       = True
    available command    = let i = coordOfCommand command in notUnderConstruction i && notTaken i
    notUnderConstruction = (not . (flip containsSite constructionSites))
    notTaken             = (not . (flip definedAt) towerMap')
    affordableMoves      = switchMovesOponentCanAfford energy' energyGenPerTurn'
    constructionSites    = buildingConstructionSites constructionQueue'

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

unwrapEvaluator :: FloatEvaluator -> Float
unwrapEvaluator (FloatEvaluator x) = x

searchDeeper :: RandomGen g => MVar Command -> g -> GameState -> IO ()
searchDeeper best g initialState = searchDeeperIter g initialScores
  where
    searchDeeperIter :: RandomGen g => g -> UV.Vector Float -> IO ()
    searchDeeperIter h scores = do
      putStrLn "Tick"
      let (h', h'')        = split h
      let newScores        = UV.zipWith (playOnceToEnd h' initialState) scores moves
      let indexOfBestSoFar = UV.maxIndex newScores
      let bestSoFarThunk   = toCommand (moves `UV.unsafeIndex` indexOfBestSoFar)
      bestSoFar           <- evaluate (bestSoFarThunk `using` rdeepseq)
      putMVar best $ (bestSoFar `deepseq` bestSoFar)
      searchDeeperIter h'' newScores
    moveCount     = UV.length moves
    moves         = myAvailableMoves initialState
    initialScores = UV.replicate moveCount 0.0

playOnceToEnd :: RandomGen g => g -> GameState -> Float -> EfficientCommand -> Float
playOnceToEnd g initialState score firstMove =
  score + (unwrapEvaluator $ playToEnd g initialState firstMove)

depth :: Int
depth = 100

playToEnd :: RandomGen g => g -> GameState -> EfficientCommand -> FloatEvaluator
playToEnd g initialState firstMove =
  let (g', initialScore, initialMoveMade) = initialAdvanceState g firstMove initialState
  in playToEndIter depth initialScore g' initialMoveMade
  where
    playToEndIter :: RandomGen g => Int -> Float -> g -> GameState -> FloatEvaluator
    playToEndIter 0 score! h currentState = FloatEvaluator $ score +  myFinalBoardScore currentState
    playToEndIter n score! h currentState =
      if gameOver currentState
      then FloatEvaluator $ if iWon currentState
                            then myFinalBoardScore currentState + score
                            else 0
      else let (h', score', newState) = advanceState h currentState
           in playToEndIter (n - 1) (score + score') h' newState

gameOver :: GameState -> Bool
gameOver (GameState { me      = (Player { health = myHealth }),
                      oponent = (Player { health = oponentsHealth }) }) =
  myHealth <= 0 || oponentsHealth <= 0

iWon :: GameState -> Bool
iWon (GameState { me      = (Player { health = myHealth }),
                  oponent = (Player { health = oponentsHealth }) }) =
  oponentsHealth <= 0 && myHealth > 0

initialAdvanceState :: RandomGen g => g -> EfficientCommand -> GameState -> (g, Float, GameState)
initialAdvanceState g firstMove gameState =
  (g',
   (myIntermediateBoardScore $ updateMyMove firstMove gameState) - oponentsAverageScore,
   updateMyMove firstMove $ updateOponentsMove oponentsMove $ tickEngine gameState)
  where
    oponentsAverageScore = ((UV.sum oponentsScores) / (fromIntegral $ UV.length oponentsMoves))
    (oponentsMove, g') =
      chooseOne g oponentsMoves $
      cdf $
      invertScores $
      oponentsScores
    oponentsScores = UV.map (myIntermediateBoardScore . (flip updateOponentsMove gameState)) oponentsMoves
    oponentsMoves = oponentsAvailableMoves gameState

advanceState :: RandomGen g => g -> GameState -> (g, Float, GameState)
advanceState g gameState =
  (g''',
   myAverageScore - oponentsAverageScore,
   updateMyMove myMove $ updateOponentsMove oponentsMove $ tickEngine gameState)
  where
    (g', g'') = split g
    myAverageScore = ((UV.sum myScores) / (fromIntegral $ UV.length myMoves))
    (myMove, _)         =
      chooseOne g' myMoves $
      cdf $
      myScores
    myScores = UV.map (myIntermediateBoardScore . (flip updateMyMove gameState)) myMoves
    myMoves = myAvailableMoves gameState
    oponentsAverageScore = ((UV.sum oponentsScores) / (fromIntegral $ UV.length oponentsMoves))
    (oponentsMove, g''') =
      chooseOne g'' oponentsMoves $
      cdf $
      invertScores $
      oponentsScores
    oponentsScores = UV.map (myIntermediateBoardScore . (flip updateOponentsMove gameState)) oponentsMoves
    oponentsMoves = oponentsAvailableMoves gameState

invertScores :: UV.Vector Float -> UV.Vector Float
invertScores = UV.map (1.0 /)

cdf :: UV.Vector Float -> UV.Vector Float
cdf xs = normalised
  where
    normalised = UV.map (/ (UV.head summed)) summed
    summed     = (UV.reverse . UV.scanl1 (+)) adjusted
    adjusted   = UV.map (+minValue) xs
    minValue   = abs $ UV.minimum xs

chooseOne :: (RandomGen g) => g -> Moves -> UV.Vector Float -> (EfficientCommand, g)
chooseOne g moves scores =
  (scanForValue scores, g')
  where
    (_, max')     = genRange g
    floatingMax   = fromIntegral max'
    normalise     = (/ floatingMax) . fromIntegral . abs
    (value, g')   = next g
    normalised    = normalise value
    numberOfMoves = UV.length moves
    indexOfLast   = numberOfMoves - 1
    indexMoves i  = moves `UV.unsafeIndex` (numberOfMoves - i - 1)
    scanForValue  = indexMoves . fromJust . lastIfNothing indexOfLast . fmap ( \ x -> x - 1) . UV.findIndex ((<= normalised))

lastIfNothing :: Int -> Maybe Int -> Maybe Int
lastIfNothing _         x@(Just _) = x
lastIfNothing lastIndex Nothing    = Just lastIndex
