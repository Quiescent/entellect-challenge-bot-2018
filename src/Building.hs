module Building (tickBuildings)
  where

import Interpretor (GameState(..),
                    CellContents(..),
                    Building(..),
                    BuildingType(..),
                    SparseMap)
import GameMap
import Cell

tickBuildings :: GameState -> GameState
tickBuildings state =
  (scoreBuildings contentsWithCoords .
   generateMissiles contentsWithCoords .
   updateBuildingProgress contentsWithCoords) state
  where
    contentsWithCoords = mapContentsWithCoords state

todo = undefined

generateMissiles :: [((Int, Int), CellContents)] -> GameState -> GameState
generateMissiles contentsWithCoords state =
  state { gameMap = gameMap' }
  where
    gameMap' = foldr generateMissile (gameMap state) contentsWithCoords

generateMissile :: ((Int, Int), CellContents) -> SparseMap -> SparseMap
generateMissile (_,      (CellContents Nothing         _)) gameMap' = gameMap'
generateMissile ((x, y), (CellContents
                          (Just (Building { weaponCooldownTimeLeft = weaponCooldownTimeLeft',
                                            weaponCooldownPeriod   = weaponCooldownPeriod',
                                            buildingType           = buildingType',
                                            buildingOwner          = owner,
                                            weaponDamage           = weaponDamage',
                                            weaponSpeed            = weaponSpeed' })) _)) gameMap' =
  case (buildingType', weaponCooldownTimeLeft') of
    (ATTACK, 0) -> adjustAt (resetCooldownAndCreateMissile owner
                                                           weaponCooldownPeriod'
                                                           weaponDamage'
                                                           weaponSpeed')
                            (x, y)
                            gameMap'
    _           -> gameMap'

updateBuildingProgress :: [((Int, Int), CellContents)] -> GameState -> GameState
updateBuildingProgress contentsWithCoords state =
  state { gameMap = gameMap' }
  where
    gameMap' = foldr updateBuildingProgress' (gameMap state) contentsWithCoords

-- TODO Implement build progress update
updateBuildingProgress' :: ((Int, Int), CellContents) -> SparseMap -> SparseMap
updateBuildingProgress' (_,      (CellContents Nothing         _)) gameMap' = gameMap'
updateBuildingProgress' ((x, y), (CellContents
                          (Just (Building { buildingType         = buildingType',
                                            constructionTimeLeft = constructionTimeLeft' })) _)) gameMap' =
  case buildingType' of
    ATTACK -> adjustAt todo
                       (x, y)
                       gameMap'
    _      -> gameMap'

scoreBuildings :: [((Int, Int), CellContents)] -> GameState -> GameState
scoreBuildings contentsWithCoords state =
  state { gameMap = gameMap'}
  where
    gameMap' = foldr scoreBuilding (gameMap state) contentsWithCoords

-- TODO Score for completed ones
scoreBuilding :: ((Int, Int), CellContents) -> SparseMap -> SparseMap
scoreBuilding ((x, y), (CellContents Nothing         _)) gameMap' = gameMap'
scoreBuilding ((x, y), (CellContents
                          (Just (Building { buildingType         = buidingType',
                                            constructionTimeLeft = constructionTimeLeft' })) _)) gameMap' =
  case buidingType' of
    ATTACK -> adjustAt todo
                       (x, y)
                       gameMap'
    _      -> gameMap'
