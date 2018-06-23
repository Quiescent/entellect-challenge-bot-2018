module Cell (cellBelongsToOponent,
             cellBelongsToMe,
             cellContainsNoBuildings,
             cellContainsBuildingType,
             allCells,
             removeMissiles,
             removeMissile,
             addMissile,
             resetCooldownAndCreateMissile,
             mapBuilding,
             buildingPredicate,
             emptyCell)
  where

import Interpretor (CellContents(..),
                    BuildingType(..),
                    Building(..),
                    GameState(..),
                    GameDetails(..),
                    Missile(..),
                    PlayerType(..),
                    SparseMap)
import GameMap
import Prelude         as P
import Missile         as M

allCells :: GameState -> [(Int, Int)]
allCells (GameState {gameDetails = details}) =
  [(x, y) | x <- [0..(mapWidth details) - 1], y <- [0..(mapHeight details) - 1]]

cellContainsNoBuildings :: SparseMap -> (Int, Int) -> Bool
cellContainsNoBuildings gameMap' (x', y') =
  case getAt (x', y') gameMap' of
    Just (CellContents (Just _) _) -> False
    _                              -> True

cellBelongsToMe :: GameState -> (Int, Int) -> Bool
cellBelongsToMe (GameState {gameDetails = details}) =
  (< (div (mapWidth details) 2)) . fst

emptyCell :: CellContents
emptyCell = CellContents Nothing emptyMissiles

cellBelongsToOponent :: GameState -> (Int, Int) -> Bool
cellBelongsToOponent state = not . (cellBelongsToMe state)

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

cellContainsBuildingType :: BuildingType -> CellContents -> Bool
cellContainsBuildingType typeOfBuilding =
  isJust . fmap (((==typeOfBuilding) . buildingType)) . buildingInCell

removeMissiles :: CellContents -> CellContents
removeMissiles (CellContents building _) = CellContents building emptyMissiles

removeMissile :: Missile -> CellContents -> CellContents
removeMissile missile cellContents =
  let missiles    = missilesInCell cellContents
      newMissiles = fst $ P.foldr maybeRemove (missiles, False) missiles
  in cellContents { missilesInCell = newMissiles }
  where
    maybeRemove :: Missile -> ([Missile], Bool) -> ([Missile], Bool)
    maybeRemove missile' (missiles, True)  = (missile' : missiles, True)
    maybeRemove missile' (missiles, False)
      | missile' == missile = (missiles, True)
      | otherwise           = (missile' : missiles, False)

addMissile :: Missile -> CellContents -> CellContents
addMissile missile cellContents@(CellContents _ missiles') =
  cellContents { missilesInCell = consMissile missile missiles' }

resetCooldownAndCreateMissile :: PlayerType -> Int -> Int -> Int -> CellContents -> CellContents
resetCooldownAndCreateMissile owner' cooldown damage' speed' =
  addMissile (Missile damage' speed' owner' 0 0) . resetBuildingCooldown cooldown

resetBuildingCooldown :: Int -> CellContents -> CellContents
resetBuildingCooldown _        cellContents@(CellContents Nothing _)          = cellContents
resetBuildingCooldown cooldown cellContents@(CellContents (Just building') _) =
  cellContents { buildingInCell = Just (building' { weaponCooldownTimeLeft = cooldown }) }

mapBuilding :: (Building -> Building) -> CellContents -> CellContents
mapBuilding f contents =
  contents { buildingInCell = (fmap f $ buildingInCell contents) }

buildingPredicate :: (Building -> Bool) -> CellContents -> Bool
buildingPredicate p contents =
  case ((fmap p . buildingInCell) contents) of
    Just True -> True
    _         -> False
