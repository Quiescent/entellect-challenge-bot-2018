module AvailableMoves
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
import Engine

import qualified Data.Vector.Unboxed as UV

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

allMidToFrontDefenseTowerMoves :: Moves
allMidToFrontDefenseTowerMoves = allMovesOfType DEFENSE midToFrontCells

allMidToFrontAttackTowerMoves :: Moves
allMidToFrontAttackTowerMoves = allMovesOfType ATTACK midToFrontCells

allForwardEnergyTowerMoves :: Moves
allForwardEnergyTowerMoves = allMovesOfType ENERGY forwardCells

allMidToFrontEnergyTowerMoves :: Moves
allMidToFrontEnergyTowerMoves = allMovesOfType ENERGY midToFrontCells

usefulEnergyMoves :: Moves
usefulEnergyMoves = allTwoBackEnergyTowerMoves UV.++ allForwardEnergyTowerMoves

oponentsPotentialEnergyMoves :: Moves
oponentsPotentialEnergyMoves = allTwoBackEnergyTowerMoves UV.++ allMidToFrontEnergyTowerMoves

allTwoBackEnergyTowerMoves :: Moves
allTwoBackEnergyTowerMoves = allMovesOfType ENERGY twoBackCells

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

theMagicalRoundWhenIStopMakingEnergyTowers :: Int
theMagicalRoundWhenIStopMakingEnergyTowers = 12

-- NOTE: Assumes that attack towers cost the same as defense towers
switchMovesICanAfford :: Int -> Int -> Moves
switchMovesICanAfford =
  switchAffordableMoves usefulEnergyMoves
                        allForwardDefenseTowerMoves
                        allMidToFrontAttackTowerMoves
                        allFrontTeslaTowerMoves

openingBookMove :: GameState -> Maybe Command
openingBookMove (GameState { gameRound = gameRound',
                             me        = player@(Player { energy = energy' }) }) =
  if gameRound' <= theMagicalRoundWhenIStopMakingEnergyTowers
  then if energy' >= energyTowerCost
       then Just $ toCommand $ UV.head $ UV.filter available allBackEnergyTowerMoves
       else Just $ toCommand nothingCommand
  else Nothing
  where
    available 0       = True
    available command = let i = coordOfCommand command in availableCoord i player

myAvailableMoves :: GameState -> Moves
myAvailableMoves (GameState { me = player@(Player { energy = energy' }) }) =
  UV.filter available affordableMoves
  where
    available 0       = True
    available command = let i = coordOfCommand command in availableCoord i player
    energyGenPerTurn' = energyGenPerTurn player
    affordableMoves   = switchMovesICanAfford energy' energyGenPerTurn'

switchMovesOponentCanAfford :: Int -> Int -> Moves
switchMovesOponentCanAfford =
  switchAffordableMoves oponentsPotentialEnergyMoves
                        allMidToFrontDefenseTowerMoves
                        allMidToFrontAttackTowerMoves
                        allFrontTeslaTowerMoves

oponentsAvailableMoves :: GameState -> Moves
oponentsAvailableMoves (GameState { oponent = player@(Player { energy = energy' }) }) =
  UV.filter available affordableMoves
  where
    available 0       = True
    available command = let i = coordOfCommand command in availableCoord i player
    energyGenPerTurn' = energyGenPerTurn player
    affordableMoves   = switchMovesOponentCanAfford energy' energyGenPerTurn'

myRandomMoves :: GameState -> Moves
myRandomMoves (GameState { me = player@(Player { energy = energy' }) }) =
  UV.filter available affordableMoves
  where
    available 0       = True
    available command = let i = coordOfCommand command in availableCoord i player
    affordableMoves   = switchRandomMovesICanAfford energy'

switchRandomMovesICanAfford :: Int -> Moves
switchRandomMovesICanAfford =
  switchAffordableRandomMoves usefulEnergyMoves allMidToFrontAttackTowerMoves

oponentsRandomMoves :: GameState -> Moves
oponentsRandomMoves (GameState { oponent = player@(Player { energy = energy' }) }) =
  UV.filter available affordableMoves
  where
    available 0       = True
    available command = let i = coordOfCommand command in availableCoord i player
    affordableMoves   = switchRandomMovesOponentCanAfford energy'

switchRandomMovesOponentCanAfford :: Int -> Moves
switchRandomMovesOponentCanAfford =
  switchAffordableRandomMoves oponentsPotentialEnergyMoves allMidToFrontAttackTowerMoves

switchAffordableRandomMoves :: Moves -> Moves -> Int -> Moves
switchAffordableRandomMoves energyTowerMoves attackTowerMoves energy'
  | energy' < energyTowerCost = UV.singleton nothingCommand
  | energy' < attackTowerCost = energyTowerMoves
  | energy' < teslaTowerCost  = attackTowerMoves
  -- TODO this should be the iron curtain when that's in
  | otherwise                 = attackTowerMoves
