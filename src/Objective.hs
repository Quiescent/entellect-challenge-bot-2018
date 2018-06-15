module Objective (myBoardScore, Move(..))
  where

import Interpretor (GameState(..),
                    Missile(..),
                    GameDetails(..),
                    Command(..),
                    CellContents(..),
                    Building(..),
                    BuildingType(..),
                    BuildingPriceIndex(..),
                    Player(..),
                    SparseMap,
                    Row)
import GameMap
import Row
import Player
import Missile
import Building
import Data.Maybe
import qualified Data.Vector as V

data Move = Move { myMove       :: Command,
                   oponentsMove :: Command }
          deriving (Show)

myBoardScore :: (GameState, a) -> (Float, (GameState, a))
myBoardScore withMove@(state, _) =
  (hitsSubtractTakenAfterTime state +
   hitsDealtToOponent state -
   hitsTakenByMe state -
   myTurnsToNextTowerByTurnByMultiplier state,
    withMove)

hitsMultiplier :: Float
hitsMultiplier = 20

hitsDealtToOponent :: GameState -> Float
hitsDealtToOponent = (*hitsMultiplier) . fromIntegral . hitsTaken . oponentsPlayer

hitsTakenByMe :: GameState -> Float
hitsTakenByMe = (*hitsMultiplier) . fromIntegral . hitsTaken . myPlayer

turnsIntoFuture :: Int
turnsIntoFuture = 10

-- TODO take into account damage dealt to buildings as part of the heuristic at a significant amount less than damage to player
-- TODO take into account the players health (i.e. a win should be very highly weighted)
hitsSubtractTakenAfterTime :: GameState -> Float
hitsSubtractTakenAfterTime state@(GameState { gameDetails = gameDetails' }) =
  fromIntegral $ sum $ map count rows'
  where
    -- TODO Work with an actual row here...
    rows'     = rows $ gameMap state
    midPoint  = mapWidth gameDetails'
    gameMap'  = gameMap state
    count row =
      (healthOfOponentsBuildings - myMissilesDamage) -
      (healthOfMyBuildings       - oponentsMissilesDamage)
      where
        myBuildingsInRow'                   = myBuildingsInRow       row gameMap'
        oponentsBuildingsInRow'             = oponentsBuildingsInRow row gameMap'
        myMissilesInRow'                    = myMissilesInRow        row gameMap'
        oponentsMissilesInRow'              = oponentsMissilesInRow  row gameMap'
        healthOfOponentsBuildings           = sum $ map integrity oponentsBuildingsInRow'
        healthOfMyBuildings                 = sum $ map integrity myBuildingsInRow'
        myExtraMissilesInNTurnsDamage       = sum $ map missilesInNTurnsDamage myBuildingsInRow'
        oponentsExtraMissilesInNTurnsDamage = sum $ map missilesInNTurnsDamage oponentsBuildingsInRow'
        myMissilesDamage                    = myExtraMissilesInNTurnsDamage       + (sum $ map damage (myMissilesInRow'))
        oponentsMissilesDamage              = oponentsExtraMissilesInNTurnsDamage + (sum $ map damage (oponentsMissilesInRow'))

missilesInNTurnsDamage :: Building -> Int
missilesInNTurnsDamage (Building { weaponCooldownTimeLeft = weaponCooldownTimeLeft',
                                   weaponCooldownPeriod   = weaponCooldownPeriod',
                                   weaponDamage           = weaponDamage',
                                   buildingType           = buildingType' }) =
  if turnsIntoFuture < weaponCooldownTimeLeft' || buildingType' /= ATTACK
  then 0
  else sum $ replicate (1 + (divWithZero (turnsIntoFuture - weaponCooldownTimeLeft') weaponCooldownPeriod')) weaponDamage'

turnsToTowerMultiplier :: Float
turnsToTowerMultiplier = 1

divWithZero :: Int -> Int -> Int
divWithZero x y =
  if y == 0
  then infinity
  else div x y

infinity :: Int
infinity = 10000000

myTurnsToNextTowerByTurnByMultiplier :: GameState -> Float
myTurnsToNextTowerByTurnByMultiplier state@(GameState { gameMap = gameMap', gameDetails = gameDetails' }) =
  (turnsToTowerMultiplier *) $
  fromIntegral $
  divWithZero (mostExpensiveTower - myEnergy') energyPerTurn
  where
    rows'              = rows $ gameMap state
    priceIndex         = buildingPrices gameDetails'
    mostExpensiveTower = maximum $ map ($ priceIndex) [attackTowerCost, defenseTowerCost, energyTowerCost]
    myEnergy'          = myEnergy state
    energyPerTurn      = (+ (roundIncomeEnergy gameDetails')) $ sum $ map energyGeneratedPerTurn myBuildings
    myBuildings        = rows' >>= ((flip myBuildingsInRow) gameMap')

oponentsMissilesInRow :: Row -> SparseMap -> [Missile]
oponentsMissilesInRow row gameMap' =
  rowFoldr (accMissiles oponentsMissile) [] row

myMissilesInRow :: Row -> SparseMap -> [Missile]
myMissilesInRow row gameMap' =
  rowFoldr (accMissiles myMissile) [] row

accMissiles :: (Missile -> Bool) -> CellContents -> [Missile] -> [Missile]
accMissiles owned (CellContents _ missilesInCell') missiles =
  missiles ++ (missilesToList $ missilesFilter owned missilesInCell')

oponentsBuildingsInRow :: Row -> SparseMap -> [Building]
oponentsBuildingsInRow row gameMap' =
  rowFoldr (accBuildings oponentsBuilding) [] row

myBuildingsInRow :: Row -> SparseMap -> [Building]
myBuildingsInRow row gameMap' =
  rowFoldr (accBuildings myBuilding) [] row

accBuildings :: (Building -> Bool) -> CellContents -> [Building] -> [Building]
accBuildings owned (CellContents (Just building') _) buildings
  | owned building'        = building' : buildings
  | otherwise              = buildings
accBuildings _ _ buildings = buildings
