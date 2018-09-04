{-# LANGUAGE BangPatterns #-}

module AvailableMoves (Moves,
                       myAvailableMoves,
                       myRandomMove,
                       oponentsAvailableMoves,
                       oponentsRandomMove,
                       openingBookMove)
  where

import Coord
import Interpretor (GameState(..),
                    Command(..),
                    BuildingType(..),
                    Player(..))
import Player
import EfficientCommand
import Cell
import Magic
import VectorIndex

import System.Random

import qualified Data.Vector.Unboxed as UV

xPredicate :: (Int -> Bool) -> Coord -> Bool
xPredicate p coord =
  let x = getX coord
  in p x

type Cells = UV.Vector Coord

cells :: Cells
cells = allCells

forwardCells :: Cells
forwardCells = UV.filter (xPredicate (>= 6)) cells

midToFrontCells :: Cells
midToFrontCells = UV.filter (xPredicate (>= 2)) cells

frontCells :: Cells
frontCells = UV.filter (xPredicate (== 7)) cells

backCells :: Cells
backCells = UV.filter (xPredicate (== 0)) cells

twoBackCells :: Cells
twoBackCells = UV.filter (xPredicate (<= 1)) cells

type Moves = UV.Vector EfficientCommand

addNothingCommand :: Moves -> Moves
addNothingCommand = UV.cons nothingCommand

addIronCurtainCommand :: Moves -> Moves
addIronCurtainCommand = UV.cons ironCurtainCommand

allMovesOfType :: BuildingType -> Cells -> Moves
allMovesOfType buildingType' cells' =
  UV.map ((flip build) (buildingTypeToInt buildingType')) cells'

allBackEnergyTowerMoves :: Moves
allBackEnergyTowerMoves = allMovesOfType ENERGY backCells

allEnergyTowerMoves :: Moves
allEnergyTowerMoves = allMovesOfType ENERGY cells

allDefenseTowerMoves :: Moves
allDefenseTowerMoves = allMovesOfType DEFENSE cells

allAttackTowerMoves :: Moves
allAttackTowerMoves = allMovesOfType ATTACK cells

allMidToFrontAttackTowerMoves :: Moves
allMidToFrontAttackTowerMoves = allMovesOfType ATTACK midToFrontCells

allForwardEnergyTowerMoves :: Moves
allForwardEnergyTowerMoves = allMovesOfType ENERGY forwardCells

allFrontAttackTowerMoves :: Moves
allFrontAttackTowerMoves = allMovesOfType ATTACK frontCells

allMidToFrontEnergyTowerMoves :: Moves
allMidToFrontEnergyTowerMoves = allMovesOfType ENERGY midToFrontCells

usefulEnergyMoves :: Moves
usefulEnergyMoves = allTwoBackEnergyTowerMoves UV.++ allForwardEnergyTowerMoves

oponentsPotentialEnergyMoves :: Moves
oponentsPotentialEnergyMoves = allTwoBackEnergyTowerMoves UV.++ allMidToFrontEnergyTowerMoves

allTwoBackEnergyTowerMoves :: Moves
allTwoBackEnergyTowerMoves = allMovesOfType ENERGY twoBackCells

switchAffordableMoves :: Moves -> Moves -> Moves -> Int -> Bool -> Moves
switchAffordableMoves energyTowerMoves
                      defenseTowerMoves
                      attackTowerMoves
                      energy'
                      ironCurtainAvailable'
  | energy' < energyTowerCost = UV.singleton nothingCommand
  | energy' < attackTowerCost = addNothingCommand $ energyTowerMoves
  | energy' < ironCurtainCost = addNothingCommand $
    attackTowerMoves UV.++
    defenseTowerMoves UV.++
    energyTowerMoves
  | otherwise = (if ironCurtainAvailable' then addIronCurtainCommand else id) $
    addNothingCommand $
    attackTowerMoves UV.++
    defenseTowerMoves UV.++
    energyTowerMoves

theMagicalRoundWhenIStopMakingEnergyTowers :: Int
theMagicalRoundWhenIStopMakingEnergyTowers = 13

theRoundOfTheSneakyAttackTower :: Int
theRoundOfTheSneakyAttackTower = 10

-- NOTE: Assumes that attack towers cost the same as defense towers
switchMovesICanAfford :: Int -> Bool -> Moves
switchMovesICanAfford =
  switchAffordableMoves allEnergyTowerMoves
                        allDefenseTowerMoves
                        allAttackTowerMoves

openingBookMove :: GameState -> Maybe Command
openingBookMove (GameState { gameRound = gameRound',
                             me        = player@(Player { energy = energy' }) }) =
  if gameRound' == theRoundOfTheSneakyAttackTower
  then Just $ toCommand $ UV.head $ allFrontAttackTowerMoves
  else if gameRound' <= theMagicalRoundWhenIStopMakingEnergyTowers
       then if energy' >= energyTowerCost
            then Just $ toCommand $ UV.head $ UV.filter available allBackEnergyTowerMoves
            else Just $ toCommand nothingCommand
       else Nothing
  where
    available 0       = True
    available 4       = True
    available command = let i = coordOfCommand command in availableCoord i player

myAvailableMoves :: GameState -> Moves
myAvailableMoves (GameState { me = player@(Player { energy               = energy',
                                                    ironCurtainAvailable = ironCurtainAvailable' }) }) =
  UV.filter available affordableMoves
  where
    available 0       = True
    available 4       = True
    available command = let i = coordOfCommand command in availableCoord i player
    affordableMoves   = switchMovesICanAfford energy' ironCurtainAvailable'

switchMovesOponentCanAfford :: Int -> Bool -> Moves
switchMovesOponentCanAfford =
  switchAffordableMoves allEnergyTowerMoves
                        allDefenseTowerMoves
                        allAttackTowerMoves

oponentsAvailableMoves :: GameState -> Moves
oponentsAvailableMoves (GameState { oponent = player@(Player { energy = energy',
                                                               ironCurtainAvailable = ironCurtainAvailable'}) }) =
  UV.filter available affordableMoves
  where
    available 0       = True
    available 4       = True
    available command = let i = coordOfCommand command in availableCoord i player
    affordableMoves   = switchMovesOponentCanAfford energy' ironCurtainAvailable'

myRandomMove :: StdGen -> GameState -> (EfficientCommand, StdGen)
myRandomMove g (GameState { me = player@(Player { energy = energy',
                                                  ironCurtainAvailable = ironCurtainAvailable' }) }) =
  (nthP available randomIdx affordableMoves, g')
  where
    (x, g')           = next g
    randomIdx         = mod x numberOfAvailable
    numberOfAvailable = UV.foldl' (countAcc available) 0 affordableMoves
    available 0       = True
    available 4       = True
    available command = let i = coordOfCommand command in availableCoord i player
    affordableMoves   = switchRandomMovesICanAfford energy' ironCurtainAvailable'

switchRandomMovesICanAfford :: Int -> Bool -> Moves
switchRandomMovesICanAfford =
  switchAffordableRandomMoves usefulEnergyMoves allMidToFrontAttackTowerMoves

oponentsRandomMove :: StdGen -> GameState -> (EfficientCommand, StdGen)
oponentsRandomMove g (GameState { oponent = player@(Player { energy = energy',
                                                             ironCurtainAvailable = ironCurtainAvailable' }) }) =
  (nthP available randomIdx affordableMoves, g')
  where
    (x, g')           = next g
    randomIdx         = mod x numberOfAvailable
    numberOfAvailable = UV.foldl' (countAcc available) 0 affordableMoves
    available 0       = True
    available 4       = True
    available command = let i = coordOfCommand command in availableCoord i player
    affordableMoves   = switchRandomMovesOponentCanAfford energy' ironCurtainAvailable'

countAcc :: (EfficientCommand -> Bool) -> Int -> EfficientCommand -> Int
countAcc p count x = if p x then count + 1 else count

nthP :: (EfficientCommand -> Bool) -> Int -> Moves -> EfficientCommand
nthP p nth xs =
  nthPIter 0 nth
  where
    nthPIter !i !0
      | p (xs `uVectorIndex` i) = xs `uVectorIndex` i
      | otherwise               = nthPIter (i + 1) 0
    nthPIter !i !n
      | p (xs `uVectorIndex` i) = nthPIter (i + 1) (n - 1)
      | otherwise               = nthPIter (i + 1) n
  

switchRandomMovesOponentCanAfford :: Int -> Bool -> Moves
switchRandomMovesOponentCanAfford =
  switchAffordableRandomMoves oponentsPotentialEnergyMoves allMidToFrontAttackTowerMoves

switchAffordableRandomMoves :: Moves -> Moves -> Int -> Bool -> Moves
switchAffordableRandomMoves energyTowerMoves attackTowerMoves energy' ironCurtainAvailable'
  | energy' < energyTowerCost = UV.singleton nothingCommand
  | energy' < attackTowerCost = energyTowerMoves
  | energy' < ironCurtainCost = attackTowerMoves
  | ironCurtainAvailable'     = UV.singleton ironCurtainCommand
  | otherwise                 = attackTowerMoves
