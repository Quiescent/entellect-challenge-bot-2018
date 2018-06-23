module Engine (tickEngine)
  where

import Interpretor (GameState(..),
                    GameDetails(..),
                    TowerStats(..),
                    Row,
                    Player(..),
                    Building(..),
                    Missile(..))
import Player
import Missile
import Building
import GameMap
import GameDetails
import GameState
import Row

tickEngine :: GameDetails -> GameState -> GameState
tickEngine details = gainEnergy details . collideMissiles . tickMissiles . tickBuildings details

tickMissiles :: GameState -> GameState
tickMissiles = mapMyPlayer (moveMissiles MoveRight) . mapOponentsPlayer (moveMissiles MoveLeft)

moveMissiles :: Direction -> Player -> Player
moveMissiles direction player = mapMissiles (moveMissile direction) player

gainEnergy :: GameDetails -> GameState -> GameState
gainEnergy details =
  mapMyPlayer (incrementEnergy details) . mapOponentsPlayer (incrementEnergy details)

incrementEnergy :: GameDetails -> Player -> Player
incrementEnergy details@(GameDetails { roundIncomeEnergy = roundIncomeEnergy' }) player' =
  updateEnergy (roundIncomeEnergy' + energyFromTowers) player'
  where
    energyFromTowers = mapFold (incrementEnergyRow details) 0 $ towerMap player'

incrementEnergyRow :: GameDetails -> Row -> Int -> Int
incrementEnergyRow details row' energyAcc =
  rowFoldr (incrementEnergyAcc details) energyAcc row'

incrementEnergyAcc :: GameDetails -> Building -> Int -> Int
incrementEnergyAcc details (Building { buildingType = buildingType' }) energyAcc =
  (energyGeneratedPerTurn $ towerStats buildingType' details) + energyAcc

collideMissiles :: GameState -> GameState
collideMissiles state =
  state''
  where
    state'           = collideMissiles' oponentsMissiles me'      findRightOf updateOponentsMissiles updateMe      state
    state''          = collideMissiles' myMissiles       oponent' findLeftOf  updateMyMissiles       updateOponent state'
    me'              = me      state
    oponent'         = oponent state
    oponentsMissiles = ownedMissiles oponent'
    myMissiles       = ownedMissiles me'

collideMissiles' :: [Missile] -> Player -> CollisionDetector -> UpdateMissiles -> UpdatePlayer -> GameState -> GameState
collideMissiles' missiles player' collisionDetector updateMissiles' updatePlayer =
  updateMissiles' missilesRemaining . updatePlayer updatedPlayer
  where
    (missilesRemaining, updatedPlayer) = foldr (collideMissile collisionDetector) ([], player') missiles

collideMissile :: CollisionDetector -> Missile -> ([Missile], Player) -> ([Missile], Player)
collideMissile collisionDetector missile@(Missile { xDisp = x', yDisp = y' }) (didntCollide, player') =
  case collisionDetector (x', y') towerMap' of
    HitNothing                 -> (missile : didntCollide, player')
    HitPlayer                  -> (didntCollide,           takeDamage missileDamage player')
    HitBuilding xHit building' ->
      let damaged = damageBuilding missileDamage building'
      in case damaged of
           Nothing         -> (didntCollide, updateTowerMap (removeAt  (xHit, y')            towerMap') player')
           Just building'' -> (didntCollide, updateTowerMap (replaceAt building'' (xHit, y') towerMap') player')
  where
    missileDamage = (damage missile)
    towerMap'     = towerMap player'
