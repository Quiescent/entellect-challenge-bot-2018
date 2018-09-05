module AvailableMoves (Moves,
                       myAvailableMoves,
                       myRandomMoves,
                       oponentsAvailableMoves,
                       oponentsRandomMoves,
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
import BitSetMap

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

-- backCells :: Cells
-- backCells = UV.filter (xPredicate (== 0)) cells

twoBackCells :: Cells
twoBackCells = UV.filter (xPredicate (<= 1)) cells

fourBackCells :: Cells
fourBackCells = UV.filter (xPredicate (<= 4)) cells

type Moves = UV.Vector EfficientCommand

addNothingCommand :: Moves -> Moves
addNothingCommand = UV.cons nothingCommand

addIronCurtainCommand :: Moves -> Moves
addIronCurtainCommand = UV.cons ironCurtainCommand

allMovesOfType :: BuildingType -> Cells -> Moves
allMovesOfType buildingType' cells' =
  UV.map ((flip build) (buildingTypeToInt buildingType')) cells'

-- allBackEnergyTowerMoves :: Moves
-- allBackEnergyTowerMoves = allMovesOfType ENERGY backCells

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

allMidToFrontEnergyTowerMoves :: Moves
allMidToFrontEnergyTowerMoves = allMovesOfType ENERGY midToFrontCells

usefulEnergyMoves :: Moves
usefulEnergyMoves = allTwoBackEnergyTowerMoves UV.++ allForwardEnergyTowerMoves

oponentsPotentialEnergyMoves :: Moves
oponentsPotentialEnergyMoves = allTwoBackEnergyTowerMoves UV.++ allMidToFrontEnergyTowerMoves

allTwoBackEnergyTowerMoves :: Moves
allTwoBackEnergyTowerMoves = allMovesOfType ENERGY twoBackCells

allFourBackEnergyTowerMoves :: Moves
allFourBackEnergyTowerMoves = allMovesOfType ENERGY fourBackCells

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
theMagicalRoundWhenIStopMakingEnergyTowers = 14

-- NOTE: Assumes that attack towers cost the same as defense towers
switchMovesICanAfford :: Int -> Bool -> Moves
switchMovesICanAfford =
  switchAffordableMoves allEnergyTowerMoves
                        allDefenseTowerMoves
                        allAttackTowerMoves

missileTowerInSameRow :: Player -> EfficientCommand -> Bool
missileTowerInSameRow (Player { attack3Towers = attack3Towers',
                                attack2Towers = attack2Towers',
                                attack1Towers = attack1Towers',
                                attack0Towers = attack0Towers' }) move =
  let allAttackTowers = addAllBuildings attack3Towers'
                        (addAllBuildings attack2Towers'
                         (addAllBuildings attack1Towers'
                          (addAllBuildings attack0Towers' 0)))
      yCoord          = getY $ coordOfCommand move
      rowOfTower      = case yCoord of
        0 -> row0
        1 -> row1
        2 -> row2
        3 -> row3
        4 -> row4
        5 -> row5
        6 -> row6
        7 -> row7
        x -> error $ "Invalid row of move: " ++ show x
  in if onlyOverlappingBuildings rowOfTower allAttackTowers > 0
     then False
     else True

openingBookMove :: GameState -> Maybe Command
openingBookMove (GameState { gameRound = gameRound',
                             me        = player@(Player { energy = energy' }),
                             oponent   = oponent' }) =
  if gameRound' <= theMagicalRoundWhenIStopMakingEnergyTowers
  then if energy' >= energyTowerCost
       then Just $
            toCommand $
            UV.head $
            UV.filter (missileTowerInSameRow oponent') $
            UV.filter available allFourBackEnergyTowerMoves
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

myRandomMoves :: GameState -> Moves
myRandomMoves (GameState { me = player@(Player { energy = energy',
                                                 ironCurtainAvailable = ironCurtainAvailable'}) }) =
  UV.filter available affordableMoves
  where
    available 0       = True
    available 4       = True
    available command = let i = coordOfCommand command in availableCoord i player
    affordableMoves   = switchRandomMovesICanAfford energy' ironCurtainAvailable'

switchRandomMovesICanAfford :: Int -> Bool -> Moves
switchRandomMovesICanAfford =
  switchAffordableRandomMoves usefulEnergyMoves allMidToFrontAttackTowerMoves

oponentsRandomMoves :: GameState -> Moves
oponentsRandomMoves (GameState { oponent = player@(Player { energy = energy',
                                                            ironCurtainAvailable = ironCurtainAvailable' }) }) =
  UV.filter available affordableMoves
  where
    available 0       = True
    available 4       = True
    available command = let i = coordOfCommand command in availableCoord i player
    affordableMoves   = switchRandomMovesOponentCanAfford energy' ironCurtainAvailable'

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
