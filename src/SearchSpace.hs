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
import Control.DeepSeq (rnf)

import Debug.Trace

myCells :: V.Vector Coord
myCells = V.filter cellBelongsToMe $ allCells

oponentsCells :: V.Vector Coord
oponentsCells = V.filter cellBelongsToOponent $ allCells

addNothingCommand :: V.Vector Command -> V.Vector Command
addNothingCommand = V.cons NothingCommand

allMyEnergyTowerMoves :: V.Vector Command
allMyEnergyTowerMoves = addNothingCommand $ V.map ((flip Build) ENERGY) myCells

allOponentsEnergyTowerMoves :: V.Vector Command
allOponentsEnergyTowerMoves = addNothingCommand $ V.map ((flip Build) ENERGY) oponentsCells

optionsWithThirtyEnergy :: V.Vector BuildingType
optionsWithThirtyEnergy = V.fromList [ENERGY, DEFENSE, ATTACK]

allMyDefenseAndAttackTowerMoves :: V.Vector Command
allMyDefenseAndAttackTowerMoves =
  addNothingCommand $ myCells >>= (\ i -> V.map (Build i) optionsWithThirtyEnergy)

allOponentsDefenseAndAttackTowerMoves :: V.Vector Command
allOponentsDefenseAndAttackTowerMoves =
  addNothingCommand $ oponentsCells >>= (\ i -> V.map (Build i) optionsWithThirtyEnergy)

optionsWithThreeHundredEnergy :: V.Vector BuildingType
optionsWithThreeHundredEnergy = V.fromList [ENERGY, DEFENSE, ATTACK, TESLA]

allMyMoves :: V.Vector Command
allMyMoves =
  addNothingCommand $ myCells >>= (\ i -> V.map (Build i) optionsWithThreeHundredEnergy)

allOponentsMoves :: V.Vector Command
allOponentsMoves =
  addNothingCommand $ oponentsCells >>= (\ i -> V.map (Build i) optionsWithThreeHundredEnergy)

-- NOTE: Assumes that attack towers cost the same as defense towers
switchMovesICanAfford :: Int -> V.Vector Command
switchMovesICanAfford energy'
  | energy' < energyTowerCost = V.singleton NothingCommand
  | energy' < attackTowerCost = allMyEnergyTowerMoves
  | energy' < teslaTowerCost  = allMyDefenseAndAttackTowerMoves
  | otherwise                 = allMyMoves

myAvailableMoves :: GameState -> V.Vector Command
myAvailableMoves (GameState { me = (Player { towerMap          = towerMap',
                                             energy            = energy',
                                             constructionQueue = constructionQueue' }) }) = do
  V.filter available affordableMoves
  where
    available (Build i _)    = notUnderConstruction i && notTaken i
    available NothingCommand = True
    notUnderConstruction     = (not . (flip containsSite constructionSites))
    notTaken                 = (not . (flip definedAt) towerMap')
    affordableMoves          = switchMovesICanAfford energy'
    constructionSites        = buildingConstructionSites constructionQueue'

switchMovesOponentCanAfford :: Int -> V.Vector Command
switchMovesOponentCanAfford energy'
  | energy' < energyTowerCost = V.singleton NothingCommand
  | energy' < attackTowerCost = allOponentsEnergyTowerMoves
  | energy' < teslaTowerCost  = allOponentsDefenseAndAttackTowerMoves
  | otherwise                 = allOponentsMoves

oponentsAvailableMoves :: GameState -> V.Vector Command
oponentsAvailableMoves (GameState { me = (Player { towerMap          = towerMap',
                                                   energy            = energy',
                                                   constructionQueue = constructionQueue' }) }) =
  V.filter available affordableMoves
  where
    available (Build i _)    = notUnderConstruction i && notTaken i
    available NothingCommand = True
    notUnderConstruction     = (not . (flip containsSite constructionSites))
    notTaken                 = (not . (flip definedAt) towerMap')
    affordableMoves          = switchMovesOponentCanAfford energy'
    constructionSites        = buildingConstructionSites constructionQueue'

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

searchDeeper :: RandomGen g => MVar Command -> g -> GameState -> IO ()
searchDeeper best g initialState = searchDeeperIter g initialScores
  where
    searchDeeperIter :: RandomGen g => g -> UV.Vector Float -> IO ()
    searchDeeperIter h scores = do
      putStrLn "Tick"
      let (h', h'')        = split h
      let newScores        = V.zipWith (playOnceToEnd h' initialState) (VG.convert scores) moves
      let indexOfBestSoFar = V.maxIndex newScores
      let bestSoFar        = moves V.! indexOfBestSoFar
      putStrLn $ "Best so far: " ++ (show bestSoFar)
      putMVar best $ rnf bestSoFar `pseq` bestSoFar
      searchDeeperIter h'' $ VG.convert newScores
    moveCount     = V.length moves
    moves         = myAvailableMoves initialState
    initialScores = UV.replicate moveCount 0.0

playOnceToEnd :: RandomGen g => g -> GameState -> Float -> Command -> Float
playOnceToEnd g initialState score firstMove =
  score + playToEnd g initialState firstMove

depth :: Int
depth = 20

playToEnd :: RandomGen g => g -> GameState -> Command -> Float
playToEnd g initialState firstMove =
  let (g', initialMoveMade) = initialAdvanceState g firstMove initialState
  in playToEndIter depth g' initialMoveMade
  where
    playToEndIter :: RandomGen g => Int -> g -> GameState -> Float
    playToEndIter 0 h currentState = myBoardScore currentState
    playToEndIter n h currentState =
      if gameOver currentState
      then myBoardScore currentState
      else let (h', newState) = advanceState h currentState
           in playToEndIter (n - 1) h' newState

gameOver :: GameState -> Bool
gameOver (GameState { me      = (Player { health = myHealth }),
                      oponent = (Player { health = oponentsHealth }) }) =
  myHealth == 0 || oponentsHealth == 0

initialAdvanceState :: RandomGen g => g -> Command -> GameState -> (g, GameState)
initialAdvanceState g firstMove gameState =
  (g', updateMyMove firstMove $ updateOponentsMove oponentsMove $ tickEngine gameState)
  where
    (oponentsMove, g') =
      chooseOne g oponentsMoves $
      cdf $
      invertScores $
      VG.convert $
      V.map (myBoardScore . (flip updateOponentsMove gameState)) oponentsMoves
    oponentsMoves = oponentsAvailableMoves gameState

advanceState :: RandomGen g => g -> GameState -> (g, GameState)
advanceState g gameState =
  (g''', updateMyMove myMove $ updateOponentsMove oponentsMove $ tickEngine gameState)
  where
    (g', g'') = split g
    (myMove, _)         =
      chooseOne g' myMoves $
      cdf $
      VG.convert $
      V.map (myBoardScore . (flip updateMyMove gameState)) myMoves
    myMoves = myAvailableMoves gameState
    (oponentsMove, g''') =
      chooseOne g'' oponentsMoves $
      cdf $
      invertScores $
      VG.convert $
      V.map (myBoardScore . (flip updateOponentsMove gameState)) oponentsMoves
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

chooseOne :: (RandomGen g) => g -> V.Vector Command -> UV.Vector Float -> (Command, g)
chooseOne g moves scores =
  (scanForValue scores, g')
  where
    (_, max')     = genRange g
    floatingMax   = fromIntegral max'
    normalise     = (/ floatingMax) . fromIntegral . abs
    (value, g')   = next g
    normalised    = normalise value
    numberOfMoves = V.length moves
    indexOfLast   = numberOfMoves - 1
    indexMoves i  = moves V.! (numberOfMoves - i - 1)
    scanForValue  = indexMoves . fromJust . lastIfNothing indexOfLast . fmap ( \ x -> x - 1) . UV.findIndex ((<= normalised))

lastIfNothing :: Int -> Maybe Int -> Maybe Int
lastIfNothing _         x@(Just _) = x
lastIfNothing lastIndex Nothing    = Just lastIndex
