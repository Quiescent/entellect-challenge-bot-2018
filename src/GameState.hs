module GameState (update)
  where

import Interpretor (GameState(..),
                    CellContents(..),
                    Building(..),
                    PlayerType(..),
                    TowerStats(..),
                    BuildingType(..),
                    Command(..))
import Building
import GameMap

update :: PlayerType -> GameState -> Command -> GameState
update _      state NothingCommand                                                   = state
update player state@(GameState { gameMap = gameMap' }) (Command x' y' buildingType') =
  case (getAt (x', y') gameMap') of
    Nothing                          -> state
    (Just (CellContents (Just _) _)) -> state
    (Just (CellContents Nothing  _)) ->
      state { gameMap = adjustAt (addBuilding player state buildingType')
                                 (x', y')
                                 gameMap' }

buildingFromStats :: PlayerType -> BuildingType -> TowerStats -> Building
buildingFromStats owner buildingType' (TowerStats initialIntegrity'
                                                  constructionTime'
                                                  towerPrice'
                                                  towerWeaponDamage'
                                                  towerWeaponSpeed'
                                                  towerWeaponCooldownPeriod'
                                                  towerEnergyGeneratedPerTurn'
                                                  towerDestroyMultiplier'
                                                  towerConstructionScore')

  = Building  { integrity              = initialIntegrity',
                constructionTimeLeft   = constructionTime',
                price                  = towerPrice',
                weaponDamage           = towerWeaponDamage',
                weaponSpeed            = towerWeaponSpeed',
                weaponCooldownTimeLeft = 0,
                weaponCooldownPeriod   = towerWeaponCooldownPeriod',
                destroyMultiplier      = towerDestroyMultiplier',
                constructionScore      = towerConstructionScore',
                energyGeneratedPerTurn = towerEnergyGeneratedPerTurn',
                buildingType           = buildingType',
                buildingX              = 0,
                buildingY              = 0,
                buildingOwner          = owner }

addBuilding :: PlayerType -> GameState -> BuildingType -> CellContents -> CellContents
addBuilding player state ATTACK  contents =
  contents { buildingInCell = Just building' }
  where
    stats = attackTowerStats' state
    building' = buildingFromStats player ATTACK stats
addBuilding player state DEFENSE contents =
  contents { buildingInCell = Just building' }
  where
    stats = defenseTowerStats' state
    building' = buildingFromStats player DEFENSE stats
addBuilding player state ENERGY  contents =
  contents { buildingInCell = Just building' }
  where
    stats = energyTowerStats' state
    building' = buildingFromStats player ENERGY stats
