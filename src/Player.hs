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

import Interpretor (decrementFitness,
                    incrementFitness,
                    GameState(..),
                    BuildingType(..),
                    Command(..),
                    Player(..),
                    Missile(..),
                    Building(..),
                    BuildingType(..),
                    TowerMap)
import Coord
import GameMap
import BuildingsUnderConstruction
import Magic
import Towers
import Buildings

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
    mapWithResetBuilding   = adjustAt resetBuildingCooldown coord (towerMap owner')

resetBuildingCooldown :: Building -> Building
resetBuildingCooldown building'
  | building' == attack0 = attack3
  | building' == tesla0  = tesla10
  | otherwise            = building'

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
takeDamage damage' player'@(Player { health = health', hitsTaken = hitsTaken' }) =
  player' { health = health' - damage', hitsTaken = hitsTaken' + 1 }

buildingFromStats :: BuildingType -> Building
buildingFromStats buildingType'
  | buildingType' == TESLA   = tesla0
  | buildingType' == ATTACK  = attack0
  | buildingType' == ENERGY  = energyTower
  | buildingType' == DEFENSE = defense4

updateMove :: Command -> Player -> Player
updateMove (Deconstruct coord') player' =
  let maybeBuilding = getAt coord' $ towerMap player'
      y'            = getY coord'
  in case maybeBuilding of
    Just building' ->
      decrementFitness y' building' $
      deconstructAt coord' player'
    Nothing                               ->
      deconstructAt coord' player'
updateMove NothingCommand               player' = player'
updateMove (Build coord' buildingType') player' =
  let y' = getY coord'
  in incrementFitness y' building' $ build timeLeft coord' building' player'
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
                                                towerMap          = towerMap',
                                                energy            = energy' }) =
  if definedAt coord towerMap'
  then player
  else player { constructionQueue = addBuilding buildingUnderConstruction constructionQueue',
                energy            = energy' - towerCost building' }
  where
    buildingUnderConstruction = createBuildingUnderConstruction (timeLeft - 1) coord building'

deconstructAt :: Coord -> Player -> Player
deconstructAt coord = mapMap (removeAt coord)

decrementCooldown :: Coord -> Player -> Player
decrementCooldown coord = mapMap (adjustAt decrementCooldownOfBuilding coord)

decrementCooldownOfBuilding :: Building -> Building
decrementCooldownOfBuilding building'
  | building' == tesla10 = tesla9
  | building' == tesla9  = tesla8
  | building' == tesla8  = tesla7
  | building' == tesla7  = tesla6
  | building' == tesla6  = tesla5
  | building' == tesla5  = tesla4
  | building' == tesla4  = tesla3
  | building' == tesla3  = tesla2
  | building' == tesla2  = tesla1
  | building' == tesla1  = tesla0
  | building' == tesla0  = tesla10
  | building' == attack3 = attack2
  | building' == attack2 = attack1
  | building' == attack1 = attack0
  | building' == attack0 = attack3
  | otherwise            = building'
