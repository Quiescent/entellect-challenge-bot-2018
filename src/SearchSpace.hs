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
import qualified Data.List   as L
import qualified Data.Vector as V

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

type ItemUnderSearch = (Float, Command)

type ItemsUnderSearch = V.Vector (Float, Command)

maximumByScore :: ItemsUnderSearch -> ItemUnderSearch
maximumByScore = L.maximumBy ( \ (x, _) (y, _) -> compare x y )

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
      let newItems  = V.zipWith (playOnceToEnd initialState) items $ V.fromList hs
      let bestSoFar = maximumByScore newItems
      putStrLn $ "Best so far: " ++ (show bestSoFar)
      putMVar best $ snd $ rnf bestSoFar `pseq` bestSoFar
      searchDeeperIter h' newItems
    myMovesToCheck = myAvailableMoves initialState
    initialItems :: ItemsUnderSearch
    initialItems = V.zip (V.replicate (length myMovesToCheck) 0) myMovesToCheck

playOnceToEnd :: RandomGen g => GameState -> ItemUnderSearch -> g -> ItemUnderSearch
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
      V.map (myBoardScore) $
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
      V.map (myBoardScore) $
      myMoves gameState
    (oponentsState, g''') =
      chooseCandidate g'' $
      invertScores $
      V.map (myBoardScore) $
      oponentsMoves gameState

myMoves :: GameState -> V.Vector (GameState, Command)
myMoves state =
  V.map ( \ move -> (updateMyMove move state, move)) $ myAvailableMoves state

oponentsMoves :: GameState -> V.Vector (GameState, Command)
oponentsMoves state = do
  V.map ( \ move -> (updateOponentsMove move state, move)) $ oponentsAvailableMoves state

invertScores :: V.Vector (Float, (GameState, a)) -> V.Vector (Float, (GameState, a))
invertScores = V.map ( \ (score', x) -> (1.0 / score', x))

zipCDF :: V.Vector (Float, (GameState, a)) -> V.Vector (Float, (GameState, a))
zipCDF xs =
  V.zipWith ( \ x (_, y) -> (x, y)) normalised descending
  where
    descending = V.reverse adjusted
    normalised = V.map (/ (V.head summed)) summed
    summed     = (V.reverse . V.scanl (+) 0 . V.map fst) adjusted
    adjusted   = V.map (\ (boardScore, x) -> (minValue + boardScore, x)) xs
    minValue   = abs $ V.minimum $ V.map fst xs

chooseOne :: (RandomGen g) => g -> V.Vector (Float, (GameState, Command)) -> ((Float, (GameState, Command)), g)
chooseOne g xs =
  (scanForValue xs, g')
  where
    (_, max')    = genRange g
    floatingMax  = fromIntegral max'
    normalise    = (/ floatingMax) . fromIntegral . abs
    (value, g')  = next g
    normalised   = normalise value
    scanForValue = fromJust . lastIfNothing xs . V.find ((<= normalised) . fst)

lastIfNothing :: V.Vector (Float, (GameState, Command)) -> Maybe (Float, (GameState, Command)) -> Maybe (Float, (GameState, Command))
lastIfNothing _  x@(Just _) = x
lastIfNothing xs Nothing    = Just $ V.last xs
