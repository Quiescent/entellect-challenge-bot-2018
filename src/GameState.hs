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

update :: GameState -> Command -> GameState
update state NothingCommand                                                   = state
update state@(GameState { gameMap = gameMap' }) (Command x' y' buildingType') =
  case (getAt (x', y') gameMap') of
    Nothing                          -> state
    (Just (CellContents (Just _) _)) -> state
    (Just (CellContents Nothing  _)) ->
      state { gameMap = adjustAt (addBuilding state buildingType')
                                 (x', y')
                                 gameMap' }

todo = undefined

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

             

-- TODO Pass the building player down
addBuilding :: GameState -> BuildingType -> CellContents -> CellContents
addBuilding state ATTACK  contents = todo
  where
    stats = attackTowerStats' state
addBuilding state DEFENSE contents = todo
  where
    stats = defenseTowerStats' state
addBuilding state ENERGY  contents = todo
  where
    stats = energyTowerStats' state
