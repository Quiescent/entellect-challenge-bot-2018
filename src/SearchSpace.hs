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

myCells :: UV.Vector Coord
myCells = UV.filter cellBelongsToMe $ allCells

oponentsCells :: UV.Vector Coord
oponentsCells = UV.filter cellBelongsToOponent $ allCells

addNothingCommand :: UV.Vector EfficientCommand -> UV.Vector EfficientCommand
addNothingCommand = UV.cons nothingCommand

allMyEnergyTowerMoves :: UV.Vector EfficientCommand
allMyEnergyTowerMoves = addNothingCommand $ UV.map ((flip build) (buildingTypeToInt ENERGY)) myCells

allOponentsEnergyTowerMoves :: UV.Vector EfficientCommand
allOponentsEnergyTowerMoves = addNothingCommand $ UV.map ((flip build) (buildingTypeToInt ENERGY)) oponentsCells

optionsWithThirtyEnergy :: UV.Vector EfficientBuilding
optionsWithThirtyEnergy = UV.fromList $ map buildingTypeToInt [ENERGY, DEFENSE, ATTACK]

createOptions :: UV.Vector EfficientBuilding -> UV.Vector Coord -> UV.Vector EfficientCommand
createOptions buildings cells =
  addNothingCommand $ UV.foldr ( \ i acc -> acc UV.++ (UV.map (build i) buildings)) UV.empty cells

allMyDefenseAndAttackTowerMoves :: UV.Vector EfficientCommand
allMyDefenseAndAttackTowerMoves = createOptions optionsWithThirtyEnergy myCells

allOponentsDefenseAndAttackTowerMoves :: UV.Vector EfficientCommand
allOponentsDefenseAndAttackTowerMoves = createOptions optionsWithThirtyEnergy oponentsCells

optionsWithThreeHundredEnergy :: UV.Vector EfficientBuilding
optionsWithThreeHundredEnergy = UV.fromList $ map buildingTypeToInt [ENERGY, DEFENSE, ATTACK, TESLA]

allMyMoves :: UV.Vector EfficientCommand
allMyMoves = createOptions optionsWithThreeHundredEnergy myCells

allOponentsMoves :: UV.Vector EfficientCommand
allOponentsMoves = createOptions optionsWithThreeHundredEnergy oponentsCells

-- NOTE: Assumes that attack towers cost the same as defense towers
switchMovesICanAfford :: Int -> UV.Vector EfficientCommand
switchMovesICanAfford energy'
  | energy' < energyTowerCost = UV.singleton nothingCommand
  | energy' < attackTowerCost = allMyEnergyTowerMoves
  | energy' < teslaTowerCost  = allMyDefenseAndAttackTowerMoves
  | otherwise                 = allMyMoves

myAvailableMoves :: GameState -> UV.Vector EfficientCommand
myAvailableMoves (GameState { me = (Player { towerMap          = towerMap',
                                             energy            = energy',
                                             constructionQueue = constructionQueue' }) }) = do
  UV.filter available affordableMoves
  where
    -- HERE change to command int
    available (-1)       = True
    available command    = let i = coordOfCommand command in notUnderConstruction i && notTaken i
    notUnderConstruction = (not . (flip containsSite constructionSites))
    notTaken             = (not . (flip definedAt) towerMap')
    affordableMoves      = switchMovesICanAfford energy'
    constructionSites    = buildingConstructionSites constructionQueue'

switchMovesOponentCanAfford :: Int -> UV.Vector EfficientCommand
switchMovesOponentCanAfford energy'
  | energy' < energyTowerCost = UV.singleton nothingCommand
  | energy' < attackTowerCost = allOponentsEnergyTowerMoves
  | energy' < teslaTowerCost  = allOponentsDefenseAndAttackTowerMoves
  | otherwise                 = allOponentsMoves

oponentsAvailableMoves :: GameState -> UV.Vector EfficientCommand
oponentsAvailableMoves (GameState { me = (Player { towerMap          = towerMap',
                                                   energy            = energy',
                                                   constructionQueue = constructionQueue' }) }) =
  UV.filter available affordableMoves
  where
    available (-1)       = True
    available command    = let i = coordOfCommand command in notUnderConstruction i && notTaken i
    notUnderConstruction = (not . (flip containsSite constructionSites))
    notTaken             = (not . (flip definedAt) towerMap')
    affordableMoves      = switchMovesOponentCanAfford energy'
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
depth = 20

playToEnd :: RandomGen g => g -> GameState -> EfficientCommand -> FloatEvaluator
playToEnd g initialState firstMove =
  let (g', initialMoveMade) = initialAdvanceState g firstMove initialState
  in playToEndIter depth g' initialMoveMade
  where
    playToEndIter :: RandomGen g => Int -> g -> GameState -> FloatEvaluator
    playToEndIter 0 h currentState = FloatEvaluator $ myBoardScore currentState
    playToEndIter n h currentState =
      if gameOver currentState
      then FloatEvaluator $ myBoardScore currentState
      else let (h', newState) = advanceState h currentState
           in playToEndIter (n - 1) h' newState

gameOver :: GameState -> Bool
gameOver (GameState { me      = (Player { health = myHealth }),
                      oponent = (Player { health = oponentsHealth }) }) =
  myHealth == 0 || oponentsHealth == 0

initialAdvanceState :: RandomGen g => g -> EfficientCommand -> GameState -> (g, GameState)
initialAdvanceState g firstMove gameState =
  (g', updateMyMove firstMove $ updateOponentsMove oponentsMove $ tickEngine gameState)
  where
    (oponentsMove, g') =
      chooseOne g oponentsMoves $
      cdf $
      invertScores $
      UV.map (myBoardScore . (flip updateOponentsMove gameState)) oponentsMoves
    oponentsMoves = oponentsAvailableMoves gameState

advanceState :: RandomGen g => g -> GameState -> (g, GameState)
advanceState g gameState =
  (g''', updateMyMove myMove $ updateOponentsMove oponentsMove $ tickEngine gameState)
  where
    (g', g'') = split g
    (myMove, _)         =
      chooseOne g' myMoves $
      cdf $
      UV.map (myBoardScore . (flip updateMyMove gameState)) myMoves
    myMoves = myAvailableMoves gameState
    (oponentsMove, g''') =
      chooseOne g'' oponentsMoves $
      cdf $
      invertScores $
      UV.map (myBoardScore . (flip updateOponentsMove gameState)) oponentsMoves
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

chooseOne :: (RandomGen g) => g -> UV.Vector EfficientCommand -> UV.Vector Float -> (EfficientCommand, g)
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
