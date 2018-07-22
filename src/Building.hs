module Building (tickBuildings, missileDamagesBuilding)
  where

import Interpretor (GameState(..),
                    Building(..),
                    BuildingType(..),
                    Player(..))
import Player
import GameMap
import GameState
import BuildingsUnderConstruction
import Magic
import Buildings

import qualified Data.Vector as V

tickBuildings :: GameState -> GameState
tickBuildings = (generateMissilesAndUpdateCooldown) . updateBuildingProgress

generateMissilesAndUpdateCooldown :: GameState -> GameState
generateMissilesAndUpdateCooldown =
  mapMyPlayer generateAndUpdateCooldownMissilesOnPlayer . mapOponentsPlayer generateAndUpdateCooldownMissilesOnPlayer

generateAndUpdateCooldownMissilesOnPlayer ::  Player -> Player
generateAndUpdateCooldownMissilesOnPlayer player =
  mapFoldIndexed generateMissilesAndUpdateCooldownForBuilding player playerMap
  where
    playerMap = towerMap player

generateMissilesAndUpdateCooldownForBuilding :: Int -> Building -> Player -> Player
generateMissilesAndUpdateCooldownForBuilding coord building' player
  | building' == attack0 = resetCooldownAndCreateMissile player coord attackTowerCooldownTime
  | building' == attack1 = decrementCooldown coord player
  | building' == attack2 = decrementCooldown coord player
  | building' == attack3 = decrementCooldown coord player
  | otherwise = player

updateBuildingProgress :: GameState -> GameState
updateBuildingProgress =
  mapMyPlayer updateBuildingProgress' . mapOponentsPlayer updateBuildingProgress'

updateBuildingProgress' :: Player -> Player
updateBuildingProgress' player =
  let (constructed, newConstructionQueue) = tickConstruction $ constructionQueue player
      newTowerMap                         = foldr placeBuilding (towerMap player) constructed
  in player { constructionQueue = newConstructionQueue,
              towerMap          = newTowerMap }

missileDamagesBuilding :: Building -> Maybe Building
missileDamagesBuilding building' =
  results `V.unsafeIndex` building'
  where
    results = V.fromList $ map inner [energyTower..tesla0]
    inner building''
      | building'' == defense4 = Just defense3
      | building'' == defense3 = Just defense2
      | building'' == defense2 = Just defense1
      | otherwise              = Nothing
