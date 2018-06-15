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
                    Row)
import GameMap
import Row
import Player
import Missile
import Building

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
hitsSubtractTakenAfterTime state =
  fromIntegral $ sum $ map count rows'
  where
    rows'     = rows $ gameMap state
    count row =
      (healthOfOponentsBuildings - myMissilesDamage) -
      (healthOfMyBuildings       - oponentsMissilesDamage)
      where
        myBuildingsInRow'                   = myBuildingsInRow            row
        oponentsBuildingsInRow'             = oponentsBuildingsInRow      row
        myMissilesInRowDamage'              = myMissilesInRowDamage       row
        oponentsMissilesInRowDamage'        = oponentsMissilesInRowDamage row
        healthOfOponentsBuildings           = sum $ map integrity oponentsBuildingsInRow'
        healthOfMyBuildings                 = sum $ map integrity myBuildingsInRow'
        myExtraMissilesInNTurnsDamage       = sum $ map missilesInNTurnsDamage myBuildingsInRow'
        oponentsExtraMissilesInNTurnsDamage = sum $ map missilesInNTurnsDamage oponentsBuildingsInRow'
        myMissilesDamage                    = myExtraMissilesInNTurnsDamage       + myMissilesInRowDamage'
        oponentsMissilesDamage              = oponentsExtraMissilesInNTurnsDamage + oponentsMissilesInRowDamage'

missilesInNTurnsDamage :: Building -> Int
missilesInNTurnsDamage (Building { weaponCooldownTimeLeft = weaponCooldownTimeLeft',
                                   weaponCooldownPeriod   = weaponCooldownPeriod',
                                   weaponDamage           = weaponDamage',
                                   buildingType           = buildingType' }) =
  if turnsIntoFuture < weaponCooldownTimeLeft' || buildingType' /= ATTACK
  then 0
  else (1 + (divWithZero (turnsIntoFuture - weaponCooldownTimeLeft') weaponCooldownPeriod')) * weaponDamage'

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
myTurnsToNextTowerByTurnByMultiplier state@(GameState { gameDetails = gameDetails' }) =
  (turnsToTowerMultiplier *) $
  fromIntegral $
  divWithZero (mostExpensiveTower - myEnergy') energyPerTurn
  where
    rows'              = rows $ gameMap state
    priceIndex         = buildingPrices gameDetails'
    mostExpensiveTower = maximum $ map ($ priceIndex) [attackTowerCost, defenseTowerCost, energyTowerCost]
    myEnergy'          = myEnergy state
    energyPerTurn      = (+ (roundIncomeEnergy gameDetails')) $ sum $ map energyGeneratedPerTurn myBuildings
    myBuildings        = rows' >>= myBuildingsInRow

oponentsMissilesInRowDamage :: Row -> Int
oponentsMissilesInRowDamage row =
  rowFoldr (accMissilesDamage oponentsMissile) 0 row

myMissilesInRowDamage :: Row -> Int
myMissilesInRowDamage row =
  rowFoldr (accMissilesDamage myMissile) 0 row

accMissilesDamage :: (Missile -> Bool) -> CellContents -> Int -> Int
accMissilesDamage owned (CellContents _ missilesInCell') damage' =
  missilesFoldr ( \ missile damageAcc -> damageAcc + if (owned missile) then damage missile else 0) damage' missilesInCell'

oponentsBuildingsInRow :: Row -> [Building]
oponentsBuildingsInRow row =
  rowFoldr (accBuildings oponentsBuilding) [] row

myBuildingsInRow :: Row -> [Building]
myBuildingsInRow row =
  rowFoldr (accBuildings myBuilding) [] row

accBuildings :: (Building -> Bool) -> CellContents -> [Building] -> [Building]
accBuildings owned (CellContents (Just building') _) buildings
  | owned building'        = building' : buildings
  | otherwise              = buildings
accBuildings _ _ buildings = buildings
