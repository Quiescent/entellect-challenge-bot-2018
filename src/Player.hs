module Player (updateEnergy,
               myPlayer,
               oponentsPlayer,
               myEnergy,
               oponentsEnergy,
               constructionTime,
               myHealth,
               oponentsHealth,
               resetCooldownAndCreateMissile,
               mapMissiles,
               incrementHitsTaken,
               updateTowerMap,
               takeDamage,
               buildingFromStats,
               updateMissiles,
               mapMap,
               build,
               updateMove,
               deconstructAt,
               decrementCooldown)
  where

import Interpretor (GameState(..),
                    BuildingType(..),
                    Command(..),
                    Player(..),
                    Missile(..),
                    Building(..),
                    TowerMap)
import Coord
import GameMap
import BuildingsUnderConstruction
import Magic

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

incrementHitsTaken :: Player -> Player
incrementHitsTaken player'@(Player { hitsTaken = hitsTaken' }) =
  player' { hitsTaken = hitsTaken' + 1 }

resetCooldownAndCreateMissile :: Player -> Coord -> Int -> Player
resetCooldownAndCreateMissile owner' coord cooldown =
  addMissile (Missile x' y') ownerWithResetBuilding
  where
    (x', y')               = fromCoord coord
    ownerWithResetBuilding = owner' { towerMap = mapWithResetBuilding }
    mapWithResetBuilding   = adjustAt (resetBuildingCooldown cooldown) coord (towerMap owner')

resetBuildingCooldown :: Int -> Building -> Building
resetBuildingCooldown cooldown building' =
  (building' { weaponCooldownTimeLeft = cooldown })

addMissile :: Missile -> Player -> Player
addMissile missile player@(Player { ownedMissiles = missiles' }) =
  player { ownedMissiles = missile : missiles' }

mapMap :: (TowerMap -> TowerMap) -> Player -> Player
mapMap f player@(Player { towerMap = towerMap' }) =
  player { towerMap = f towerMap' }

mapMissiles :: (Missile -> Missile) -> Player -> Player
mapMissiles f player@(Player { ownedMissiles = ownedMissiles' }) =
  player { ownedMissiles = map f ownedMissiles' }

updateMissiles :: [Missile] -> Player -> Player
updateMissiles missiles player = player { ownedMissiles = missiles }

updateTowerMap :: TowerMap -> Player -> Player
updateTowerMap towerMap' player' = player' { towerMap = towerMap' }

takeDamage :: Int -> Player -> Player
takeDamage damage' player'@(Player { health = health' }) =
  player' { health = health' - damage' }

buildingFromStats :: BuildingType -> Building
buildingFromStats TESLA   = teslaTower
buildingFromStats ATTACK  = attackTower
buildingFromStats ENERGY  = energyTower
buildingFromStats DEFENSE = defenseTower

teslaTower :: Building
teslaTower = (Building teslaTowerHealth 0 TESLA)

attackTower :: Building
attackTower = (Building attackTowerHealth 0 ATTACK)

defenseTower :: Building
defenseTower = (Building defenseTowerHealth 0 DEFENSE)

energyTower :: Building
energyTower = (Building energyTowerHealth 0 ENERGY)

updateMove :: Command -> Player -> Player
updateMove (Deconstruct x' y')         = deconstructAt $ toCoord x' y'
updateMove NothingCommand              = id
updateMove (Build x' y' buildingType') = build timeLeft (toCoord x' y') building'
  where
    timeLeft  = constructionTime buildingType'
    building' = buildingFromStats buildingType'

constructionTime :: BuildingType -> Int
constructionTime TESLA   = teslaTowerConstructionTime
constructionTime ENERGY  = energyTowerConstructionTime
constructionTime DEFENSE = defenseTowerConstructionTime
constructionTime ATTACK  = attackTowerConstructionTime

build :: Int -> Coord -> Building -> Player -> Player
build timeLeft coord building' player@(Player { constructionQueue = constructionQueue',
                                                towerMap          = towerMap' }) =
  if definedAt coord towerMap'
  then player
  else player { constructionQueue = addBuilding buildingUnderConstruction constructionQueue' }
  where
    buildingUnderConstruction = createBuildingUnderConstruction timeLeft coord building'

deconstructAt :: Coord -> Player -> Player
deconstructAt coord = mapMap (removeAt coord)

decrementCooldown :: Coord -> Player -> Player
decrementCooldown coord = mapMap (adjustAt decrementCooldownOfBuilding coord)

decrementCooldownOfBuilding :: Building -> Building
decrementCooldownOfBuilding building@(Building { weaponCooldownTimeLeft = cooldown }) =
  building { weaponCooldownTimeLeft = cooldown - 1 }
