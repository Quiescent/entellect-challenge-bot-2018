module Cell (cellBelongsToOponent,
             cellBelongsToMe,
             cellIsEmpty,
             cellContainsBuildingType,
             allCells,
             removeMissiles,
             removeMissile,
             addMissile,
             resetCooldownAndCreateMissile,
             mapBuilding,
             buildingPredicate)
  where

import Interpretor (CellContents(..),
                    BuildingType(..),
                    Building(..),
                    GameState(..),
                    GameDetails(..),
                    Missile(..),
                    PlayerType(..),
                    SparseMap)
import Data.Map.Strict as M
import Data.Vector     as V
import Prelude         as P

allCells :: GameState -> [(Int, Int)]
allCells (GameState {gameDetails = details}) =
  [(x, y) | x <- [0..(mapWidth details) - 1], y <- [0..(mapHeight details) - 1]]

cellIsEmpty :: SparseMap -> (Int, Int) -> Bool
cellIsEmpty = flip M.member

-- TODO: which is width and which is height? (for both of the next
-- two)
cellBelongsToMe :: GameState -> (Int, Int) -> Bool
cellBelongsToMe (GameState {gameDetails = details}) =
  (< (div (mapWidth details) 2)) . snd

cellBelongsToOponent :: GameState -> (Int, Int) -> Bool
cellBelongsToOponent state = not . (cellBelongsToMe state)

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

cellContainsBuildingType :: BuildingType -> CellContents -> Bool
cellContainsBuildingType typeOfBuilding =
  isJust . fmap (((==typeOfBuilding) . buildingType)) . buildingInCell

removeMissiles :: CellContents -> CellContents
removeMissiles (CellContents building _) = CellContents building V.empty

removeMissile :: Missile -> CellContents -> CellContents
removeMissile missile cellContents =
  let missiles    = V.toList $ missilesInCell cellContents
      newMissiles = V.fromList $ fst $ P.foldr maybeRemove (missiles, False) missiles
  in cellContents { missilesInCell = newMissiles }
  where
    maybeRemove :: Missile -> ([Missile], Bool) -> ([Missile], Bool)
    maybeRemove missile' (missiles, True)  = (missile' : missiles, True)
    maybeRemove missile' (missiles, False)
      | missile' == missile = (missiles, True)
      | otherwise           = (missile' : missiles, False)

addMissile :: Missile -> CellContents -> CellContents
addMissile missile cellContents@(CellContents _ missiles') =
  cellContents { missilesInCell = V.cons missile missiles' }

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
