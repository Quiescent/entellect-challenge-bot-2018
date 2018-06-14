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
                    SparseMap)
import GameMap
import Player
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

hitsSubtractTakenAfterTime :: GameState -> Float
hitsSubtractTakenAfterTime state@(GameState { gameDetails = gameDetails' }) =
  fromIntegral $ sum $ map count rows
  where
    -- TODO Work with an actual row here...
    rows      = [0..(mapHeight gameDetails') - 1]
    midPoint  = mapWidth gameDetails'
    gameMap'  = gameMap state
    count row =
      (healthOfOponentsBuildings - myMissilesDamage) -
      (healthOfMyBuildings       - oponentsMissilesDamage)
      where
        myBuildingsInRow'                   = myBuildingsInRow       row midPoint gameMap'
        oponentsBuildingsInRow'             = oponentsBuildingsInRow row midPoint gameMap'
        myMissilesInRow'                    = myMissilesInRow        row midPoint gameMap'
        oponentsMissilesInRow'              = oponentsMissilesInRow  row midPoint gameMap'
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
  (turnsToTowerMultiplier *) $ fromIntegral $ divWithZero (mostExpensiveTower - myEnergy') energyPerTurn
  where
    priceIndex         = buildingPrices gameDetails'
    mostExpensiveTower = maximum $ map ($ priceIndex) [attackTowerCost, defenseTowerCost, energyTowerCost]
    myEnergy'          = myEnergy state
    energyPerTurn      = (+ (roundIncomeEnergy gameDetails')) $ sum $ map energyGeneratedPerTurn myBuildings
    myBuildings        = [0..(mapHeight gameDetails') - 1] >>= ( \ row -> myBuildingsInRow row midPoint gameMap')
    midPoint           = mapWidth gameDetails'

-- Just don't look down here.... ok...?
----------------------------------------------------------------------------------------------------

cellHasBuilding :: (Int, Int) -> SparseMap -> Bool
cellHasBuilding coord gameMap' =
  (definedAt coord gameMap') &&
  (isJust $ buildingInCell $ fromJust $ getAt coord gameMap')

cellHasMissile :: (Int, Int) -> SparseMap -> Bool
cellHasMissile coord gameMap' =
  (definedAt coord gameMap') &&
  (not $ V.null $ missilesInCell $ fromJust $ getAt coord gameMap')

myMissilesInRow :: Int -> Int -> SparseMap -> [Missile]
myMissilesInRow row midPoint gameMap' = do
  x       <- filter (\ x' -> cellHasMissile (x', row) gameMap') [0..(midPoint - 1)]
  missile <- V.toList $ missilesInCell $ fromJust $ getAt (x, row) gameMap'
  return missile

oponentsMissilesInRow :: Int -> Int -> SparseMap -> [Missile]
oponentsMissilesInRow row midPoint gameMap' = do
  x       <- filter (\ x' -> cellHasMissile (x', row) gameMap') [midPoint..(2 * midPoint - 1)]
  missile <- V.toList $ missilesInCell $ fromJust $ getAt (x, row) gameMap'
  return missile

oponentsBuildingsInRow :: Int -> Int -> SparseMap -> [Building]
oponentsBuildingsInRow row midPoint gameMap' = do
  x <- filter (\ x' -> cellHasBuilding (x', row) gameMap') [midPoint..(2 * midPoint - 1)]
  return $ fromJust $ buildingInCell $ fromJust $ getAt (x, row) gameMap'

-- TODO DRY!
myBuildingsInRow :: Int -> Int -> SparseMap -> [Building]
myBuildingsInRow row midPoint gameMap' = do
  x <- filter (\ x' -> cellHasBuilding (x', row) gameMap') [0..(midPoint - 1)]
  return $ fromJust $ buildingInCell $ fromJust $ getAt (x, row) gameMap'
