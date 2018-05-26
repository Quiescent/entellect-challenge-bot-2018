module Building (tickBuildings)
  where

import Interpretor (GameState(..),
                    CellContents(..),
                    Building(..),
                    BuildingType(..),
                    SparseMap)
import GameMap

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

--- TODO implement cooldown update
generateMissile :: ((Int, Int), CellContents) -> SparseMap -> SparseMap
generateMissile ((x, y), (CellContents Nothing         _)) gameMap' = gameMap'
generateMissile ((x, y), (CellContents
                          (Just (Building { weaponCooldownTimeLeft = weaponCooldownTimeLeft',
                                            weaponCooldownPeriod   = weaponCooldownPeriod',
                                            buildingType           = buildingType'})) _)) gameMap' =
  case buildingType' of
    ATTACK -> adjustAt todo
                       (x, y)
                       gameMap'
    _      -> gameMap'

updateBuildingProgress :: [((Int, Int), CellContents)] -> GameState -> GameState
updateBuildingProgress contentsWithCoords state =
  state { gameMap = gameMap' }
  where
    gameMap' = foldr updateBuildingProgress' (gameMap state) contentsWithCoords

-- TODO Implement build progress update
updateBuildingProgress' :: ((Int, Int), CellContents) -> SparseMap -> SparseMap
updateBuildingProgress' ((x, y), (CellContents Nothing         _)) gameMap' = gameMap'
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
