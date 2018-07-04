module Building (tickBuildings, damageBuilding)
  where

import Interpretor (GameState(..),
                    Building(..),
                    BuildingType(..),
                    Player(..),
                    Row)
import Row
import Player
import GameMap
import GameState
import BuildingsUnderConstruction
import Magic

tickBuildings :: GameState -> GameState
tickBuildings = (generateMissiles) . updateBuildingProgress

generateMissiles :: GameState -> GameState
generateMissiles =
  mapMyPlayer (generateMissilesOnPlayer) . mapOponentsPlayer (generateMissilesOnPlayer)

generateMissilesOnPlayer ::  Player -> Player
generateMissilesOnPlayer player =
  mapFoldIndexed (generateMissilesOnRow) player playerMap
  where
    playerMap = towerMap player

generateMissilesOnRow :: Int -> Row -> Player -> Player
generateMissilesOnRow y' row player =
  rowFoldrIndexed (generateMissilesForBuilding y') player row

generateMissilesForBuilding :: Int -> Int -> Building -> Player -> Player
generateMissilesForBuilding y' x'
                            (Building { weaponCooldownTimeLeft = weaponCooldownTimeLeft',
                                        buildingType           = buildingType' })
                            player =
  if (buildingType' /= ATTACK || weaponCooldownTimeLeft' /= 0)
  then player
  else resetCooldownAndCreateMissile player x' y' attackTowerCooldownTime

updateBuildingProgress :: GameState -> GameState
updateBuildingProgress =
  mapMyPlayer updateBuildingProgress' . mapOponentsPlayer updateBuildingProgress'

updateBuildingProgress' :: Player -> Player
updateBuildingProgress' player =
  let (constructed, newConstructionQueue) = tickConstruction $ constructionQueue player
      newTowerMap                         = foldr placeBuilding (towerMap player) constructed
  in player { constructionQueue = newConstructionQueue,
              towerMap          = newTowerMap }

damageBuilding :: Int -> Building -> Maybe Building
damageBuilding damage' building' =
  let integrity' = integrity building'
  in if integrity' <= damage'
     then Nothing
     else Just (building' { integrity = integrity' - damage' })
