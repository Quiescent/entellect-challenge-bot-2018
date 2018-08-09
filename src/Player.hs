module Player (emptyMissiles,
               updateEnergy,
               myPlayer,
               oponentsPlayer,
               myEnergy,
               oponentsEnergy,
               constructionTime,
               myHealth,
               oponentsHealth,
               takeDamage,
               buildingFromStats,
               build,
               updateMove,
               deconstructAt,
               buildOnMap,
               availableCoord,
               collideAndMoveMissiles)
  where

import Interpretor (incrementFitness,
                    GameState(..),
                    BuildingType(..),
                    Player(..),
                    Building,
                    BuildingType(..))
import Coord
import Magic
import Towers
import Buildings
import EfficientCommand
import BitSetMap


myPlayer :: GameState -> Player
myPlayer = me

oponentsPlayer :: GameState -> Player
oponentsPlayer = oponent

playerEnergy :: (GameState -> Player) -> GameState -> Int
playerEnergy player' = energy . player'

myEnergy :: GameState -> Int
myEnergy = playerEnergy myPlayer

oponentsEnergy :: GameState -> Int
oponentsEnergy = playerEnergy oponentsPlayer

playerHealth :: (GameState -> Player) -> GameState -> Int
playerHealth player' = health . player'

myHealth :: GameState -> Int
myHealth = playerHealth myPlayer

oponentsHealth :: GameState -> Int
oponentsHealth = playerHealth oponentsPlayer

updateEnergy :: Int -> Player -> Player
updateEnergy energyToAdd player@(Player { energy = energy' }) =
  player { energy = energy' + energyToAdd }

takeDamage :: Int -> Player -> Player
takeDamage damage' player'@(Player { health = health' }) =
  player' { health = health' - damage' }

buildingFromStats :: BuildingType -> Building
buildingFromStats TESLA   = Tesla0
buildingFromStats ATTACK  = Attack0
buildingFromStats ENERGY  = EnergyTower
buildingFromStats DEFENSE = Defense4

updateMove :: EfficientCommand -> Player -> Player
-- TODO: Handle deconstruct
updateMove 0       player' = player'
updateMove command player' =
  incrementFitness y' building' $ buildOnMap coord' building' player'
  where
    coord'        = coordOfCommand command
    y'            = getY coord'
    buildingType' = buildingTypeOfCommand command
    building'     = buildingFromStats buildingType'

constructionTime :: BuildingType -> Int
constructionTime TESLA   = teslaTowerConstructionTime
constructionTime ENERGY  = energyTowerConstructionTime
constructionTime DEFENSE = defenseTowerConstructionTime
constructionTime ATTACK  = attackTowerConstructionTime

buildOnMap :: Coord -> Building -> Player -> Player
buildOnMap coord building'
  player@(Player { defenseTowersUnderConstruction2 = defenseTowersUnderConstruction2',
                   energyTowersUnderConstruction   = energyTowersUnderConstruction',
                   attackTowersUnderConstruction   = attackTowersUnderConstruction',
                   teslaTower0                     = teslaTower0',
                   teslaTower1                     = teslaTower1',
                   energy                          = energy' }) =
  case building' of
    Defense4    -> player' { defenseTowersUnderConstruction2 = addBuilding coord defenseTowersUnderConstruction2' }
    Attack0     -> player' { attackTowersUnderConstruction   = addBuilding coord attackTowersUnderConstruction' }
    EnergyTower -> player' { energyTowersUnderConstruction   = addBuilding coord energyTowersUnderConstruction' }
    Tesla0      ->
      if teslaTower0' == 0
      then player' { teslaTower0                 = addBuilding coord teslaTower0',
                     teslaTower0ConstructionTime = teslaTowerConstructionTime,
                     teslaTower0CooldownTime     = 0 }
      else player' { teslaTower1                 = addBuilding coord teslaTower1',
                     teslaTower1ConstructionTime = teslaTowerConstructionTime,
                     teslaTower1CooldownTime     = 0 }
    x           -> error $
      "Attempted to build an invalid building state (constructed buildings have zero CD and full health): " ++ show x
  where
    player' = player { energy = energy' - towerCost building' }

-- TODO: Implement
collideAndMoveMissiles :: Player -> Player
collideAndMoveMissiles = id

deconstructAt :: Coord -> Player -> Player
deconstructAt coord
  player@(Player { energyTowersUnderConstruction   = energyTowersUnderConstruction',
                   energyTowers                    = energyTowers',
                   attackTowersUnderConstruction   = attackTowersUnderConstruction',
                   attack3Towers                   = attack3Towers',
                   attack2Towers                   = attack2Towers',
                   attack1Towers                   = attack1Towers',
                   attack0Towers                   = attack0Towers',
                   defenseTowersUnderConstruction2 = defenseTowersUnderConstruction2',
                   defenseTowersUnderConstruction1 = defenseTowersUnderConstruction1',
                   defenseTowersUnderConstruction0 = defenseTowersUnderConstruction0',
                   defense4Towers                  = defense4Towers',
                   defense3Towers                  = defense3Towers',
                   defense2Towers                  = defense2Towers',
                   defense1Towers                  = defense1Towers',
                   teslaTower0                     = teslaTower0',
                   teslaTower1                     = teslaTower1' })
  | containsBuildingAt coord energyTowersUnderConstruction'   =
    player { energyTowersUnderConstruction = removeBuilding coord energyTowersUnderConstruction' }
  | containsBuildingAt coord energyTowers'                    =
    player { energyTowers = removeBuilding coord energyTowers' }
  | containsBuildingAt coord attackTowersUnderConstruction'   =
    player { attackTowersUnderConstruction = removeBuilding coord attackTowersUnderConstruction' }
  | containsBuildingAt coord attack3Towers'                   =
    player { attack3Towers = removeBuilding coord attack3Towers' }
  | containsBuildingAt coord attack2Towers'                   =
    player { attack2Towers = removeBuilding coord attack2Towers' }
  | containsBuildingAt coord attack1Towers'                   =
    player { attack1Towers = removeBuilding coord attack1Towers' }
  | containsBuildingAt coord attack0Towers'                   =
    player { attack0Towers = removeBuilding coord attack0Towers' }
  | containsBuildingAt coord defenseTowersUnderConstruction2' =
    player { defenseTowersUnderConstruction2 = removeBuilding coord defenseTowersUnderConstruction2' }
  | containsBuildingAt coord defenseTowersUnderConstruction1' =
    player { defenseTowersUnderConstruction1 = removeBuilding coord defenseTowersUnderConstruction1' }
  | containsBuildingAt coord defenseTowersUnderConstruction0' =
    player { defenseTowersUnderConstruction0 = removeBuilding coord defenseTowersUnderConstruction0' }
  | containsBuildingAt coord defense4Towers'                  =
    player { defense4Towers = removeBuilding coord defense4Towers' }
  | containsBuildingAt coord defense3Towers'                  =
    player { defense3Towers = removeBuilding coord defense3Towers' }
  | containsBuildingAt coord defense2Towers'                  =
    player { defense2Towers = removeBuilding coord defense2Towers' }
  | containsBuildingAt coord defense1Towers'                  =
    player { defense1Towers = removeBuilding coord defense1Towers' }
  | containsBuildingAt coord teslaTower0'                     =
    player { teslaTower0 = removeBuilding coord teslaTower0' }
  | containsBuildingAt coord teslaTower1'                     =
    player { teslaTower1 = removeBuilding coord teslaTower1' }
  | otherwise                                                 = player

availableCoord :: Coord -> Player -> Bool
availableCoord coord
  (Player { energyTowersUnderConstruction   = energyTowersUnderConstruction',
            energyTowers                    = energyTowers',
            attackTowersUnderConstruction   = attackTowersUnderConstruction',
            attack3Towers                   = attack3Towers',
            attack2Towers                   = attack2Towers',
            attack1Towers                   = attack1Towers',
            attack0Towers                   = attack0Towers',
            defenseTowersUnderConstruction2 = defenseTowersUnderConstruction2',
            defenseTowersUnderConstruction1 = defenseTowersUnderConstruction1',
            defenseTowersUnderConstruction0 = defenseTowersUnderConstruction0',
            defense4Towers                  = defense4Towers',
            defense3Towers                  = defense3Towers',
            defense2Towers                  = defense2Towers',
            defense1Towers                  = defense1Towers',
            teslaTower0                     = teslaTower0',
            teslaTower1                     = teslaTower1' }) =
  not $ containsBuildingAt coord
  (energyTowersUnderConstruction'   `addAllBuildings`
   energyTowers'                    `addAllBuildings`
   attackTowersUnderConstruction'   `addAllBuildings`
   attack3Towers'                   `addAllBuildings`
   attack2Towers'                   `addAllBuildings`
   attack1Towers'                   `addAllBuildings`
   attack0Towers'                   `addAllBuildings`
   defenseTowersUnderConstruction2' `addAllBuildings`
   defenseTowersUnderConstruction1' `addAllBuildings`
   defenseTowersUnderConstruction0' `addAllBuildings`
   defense4Towers'                  `addAllBuildings`
   defense3Towers'                  `addAllBuildings`
   defense2Towers'                  `addAllBuildings`
   defense1Towers'                  `addAllBuildings`
   teslaTower0'                     `addAllBuildings`
   teslaTower1')
