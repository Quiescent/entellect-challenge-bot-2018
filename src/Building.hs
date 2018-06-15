module Building (myBuilding,
                 oponentsBuilding,
                 tickBuildings,
                 attackTowerStats',
                 defenseTowerStats',
                 energyTowerStats',
                 buildingIsConstructed)
  where

import Interpretor (GameState(..),
                    CellContents(..),
                    Building(..),
                    BuildingType(..),
                    PlayerType(..),
                    BuildingStats(..),
                    GameDetails(..),
                    TowerStats(..),
                    SparseMap)
import Player
import GameMap
import Cell

tickBuildings :: GameState -> GameState
tickBuildings state =
  (scoreBuildings contentsWithCoords .
   generateMissiles contentsWithCoords .
   updateBuildingProgress contentsWithCoords) state
  where
    contentsWithCoords = mapContentsWithCoords state

generateMissiles :: [((Int, Int), CellContents)] -> GameState -> GameState
generateMissiles contentsWithCoords state =
  state { gameMap = gameMap' }
  where
    gameMap' = foldr generateMissile (gameMap state) contentsWithCoords

-- TODO DRY(!)
buildingInCellIsConstructed :: CellContents -> Bool
buildingInCellIsConstructed = buildingPredicate buildingIsConstructed

generateMissile :: ((Int, Int), CellContents) -> SparseMap -> SparseMap
generateMissile (_,               (CellContents Nothing _)) gameMap' = gameMap'
generateMissile ((x, y), contents@(CellContents
                          (Just (Building { weaponCooldownTimeLeft = weaponCooldownTimeLeft',
                                            weaponCooldownPeriod   = weaponCooldownPeriod',
                                            buildingType           = buildingType',
                                            buildingOwner          = owner,
                                            weaponDamage           = weaponDamage',
                                            weaponSpeed            = weaponSpeed' })) _))
                gameMap' =
  case (buildingType', weaponCooldownTimeLeft', buildingInCellIsConstructed contents) of
    (ATTACK, 0, True) -> adjustAt (resetCooldownAndCreateMissile owner
                                                                 weaponCooldownPeriod'
                                                                 weaponDamage'
                                                                 weaponSpeed')
                                  (x, y)
                                  gameMap'
    _                 -> gameMap'

updateBuildingProgress :: [((Int, Int), CellContents)] -> GameState -> GameState
updateBuildingProgress contentsWithCoords state =
  state { gameMap = gameMap' }
  where
    gameMap' = foldr updateBuildingProgress' (gameMap state) contentsWithCoords

updateBuildingProgress' :: ((Int, Int), CellContents) -> SparseMap -> SparseMap
updateBuildingProgress' (_,      (CellContents Nothing         _)) gameMap' = gameMap'
updateBuildingProgress' ((x, y), (CellContents
                          (Just (Building { constructionTimeLeft = constructionTimeLeft' })) _))
                        gameMap' =
  if   constructionTimeLeft' /= 0
  then adjustAt decrementBuildingTimeLeft
                (x, y)
                gameMap'
  else gameMap'

decrementBuildingTimeLeft :: CellContents -> CellContents
decrementBuildingTimeLeft =
  mapBuilding ( \ building -> building { constructionTimeLeft = constructionTimeLeft building - 1 })

scoreBuildings :: [((Int, Int), CellContents)] -> GameState -> GameState
scoreBuildings contentsWithCoords =
  incrementMyPoints myPoints . incrementOponentsPoints oponentsPoints
  where
    (myPoints, oponentsPoints) = foldr scoreBuilding (0, 0) contentsWithCoords

scoreBuilding :: ((Int, Int), CellContents) -> (Int, Int) -> (Int, Int)
scoreBuilding (_, (CellContents Nothing         _)) points = points
scoreBuilding (_, (CellContents
                    (Just (Building { constructionTimeLeft = constructionTimeLeft',
                                      constructionScore    = constructionScore',
                                      buildingOwner        = owner })) _))
              points@(myPoints, oponentsPoints) =
  if constructionTimeLeft' == 1
  then if owner == A
       then (myPoints + constructionScore', oponentsPoints)
       else (myPoints,                     oponentsPoints + constructionScore')
  else points

towerStats :: (BuildingStats -> TowerStats) -> GameState -> TowerStats
towerStats stats = stats . buildingsStats . gameDetails

attackTowerStats' :: GameState -> TowerStats
attackTowerStats' = towerStats attackTowerStats

defenseTowerStats' :: GameState -> TowerStats
defenseTowerStats' = towerStats defenseTowerStats

energyTowerStats' :: GameState -> TowerStats
energyTowerStats' = towerStats energyTowerStats

buildingIsConstructed :: Building -> Bool
buildingIsConstructed = ((== -1) . constructionTimeLeft)

buildingOwnedBy :: (PlayerType -> Bool) -> Building -> Bool
buildingOwnedBy p = p . buildingOwner

myBuilding :: Building -> Bool
myBuilding = buildingOwnedBy (==A)

oponentsBuilding :: Building -> Bool
oponentsBuilding = buildingOwnedBy (==B)
