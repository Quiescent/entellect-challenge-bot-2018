module Objective (boardScore, Move(..))
  where

import Interpretor (GameState(..),
                    Missile(..),
                    GameDetails(..),
                    Command(..),
                    CellContents(..),
                    Building(..),
                    BuildingType(..),
                    BuildingPriceIndex(..),
                    PlayerType(..),
                    Player(..),
                    SparseMap)
import GameMap
import Player
import Data.Maybe
import qualified Data.Vector as V

data Move = Move { myMove       :: Command,
                   oponentsMove :: Command }
          deriving (Show)

boardScore :: (GameState, Move) -> (Float, (GameState, Move))
boardScore withMove@(state, _) =
  (hitsSubtractTakenAfterTime state +
   hitsDealtToOponent state -
   hitsTakenByMe state -
   turnsToNextTowerByTurnByMultiplier state,
    withMove)

hitsMultiplier :: Float
hitsMultiplier = 20

hitsDealtToOponent :: GameState -> Float
hitsDealtToOponent = (*hitsMultiplier) . fromIntegral . hitsTaken . oponentsPlayer

hitsTakenByMe :: GameState -> Float
hitsTakenByMe = (*hitsMultiplier) . fromIntegral . hitsTaken . myPlayer

turnsIntoFuture :: Int
turnsIntoFuture = 5

hitsSubtractTakenAfterTime :: GameState -> Float
hitsSubtractTakenAfterTime state@(GameState { gameDetails = gameDetails' }) =
  fromIntegral $ sum $ map count rows
  where
    rows      = [0..(mapHeight gameDetails') - 1]
    midPoint  = mapWidth gameDetails'
    gameMap'  = gameMap state
    count row =
      (healthOfOponentsBuildings - myMissilesDamage) -
      (healthOfMyBuildings       - oponentsMissilesDamage)
      where
        myBuildingsInRow'             = myBuildingsInRow       row midPoint gameMap'
        oponentsBuildingsInRow'       = oponentsBuildingsInRow row midPoint gameMap'
        myMissilesInRow'              = myMissilesInRow        row midPoint gameMap'
        oponentsMissilesInRow'        = oponentsMissilesInRow  row midPoint gameMap'
        healthOfOponentsBuildings     = sum $ map integrity oponentsBuildingsInRow'
        healthOfMyBuildings           = sum $ map integrity myBuildingsInRow'
        myExtraMissilesInNTurns       = myBuildingsInRow'       >>= missilesInNTurns
        oponentsExtraMissilesInNTurns = oponentsBuildingsInRow' >>= missilesInNTurns
        myMissilesDamage              = sum $ map damage (myMissilesInRow' ++ myExtraMissilesInNTurns)
        oponentsMissilesDamage        = sum $ map damage (oponentsMissilesInRow' ++ oponentsExtraMissilesInNTurns)

-- TODO This is terrible.  I'm creating missiles to count the damage they can deal...
missilesInNTurns :: Building -> [Missile]
missilesInNTurns (Building { weaponCooldownTimeLeft = weaponCooldownTimeLeft',
                             weaponCooldownPeriod   = weaponCooldownPeriod',
                             weaponDamage           = weaponDamage',
                             weaponSpeed            = weaponSpeed',
                             buildingType           = buildingType' }) =
  if turnsIntoFuture < weaponCooldownTimeLeft' || buildingType' /= ATTACK
  then []
  else replicate (1 + (divWithZero (turnsIntoFuture - weaponCooldownTimeLeft') weaponCooldownPeriod')) $
       Missile weaponDamage' -- Doesn't matter for this from here down
               weaponSpeed'
               A 
               0
               0

turnsToTowerMultiplier :: Float
turnsToTowerMultiplier = 10

divWithZero :: Int -> Int -> Int
divWithZero x y =
  if y == 0
  then infinity
  else div x y


infinity :: Int
infinity = 10000000

turnsToNextTowerByTurnByMultiplier :: GameState -> Float
turnsToNextTowerByTurnByMultiplier state@(GameState { gameMap = gameMap', gameDetails = gameDetails' }) =
  (turnsToTowerMultiplier *) $ fromIntegral $ divWithZero (mostExpensiveTower - myEnergy') energyPerTurn
  where
    priceIndex         = buildingPrices gameDetails'
    mostExpensiveTower = maximum $ map ($ priceIndex) [attackTowerCost, defenseTowerCost, energyTowerCost]
    myEnergy'          = myEnergy state
    energyPerTurn      = sum $ map energyGeneratedPerTurn myBuildings
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
