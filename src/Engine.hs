module Engine (tickEngine)
  where

import Interpretor (decrementFitness,
                    GameState(..),
                    BuildingType(..),
                    Player(..),
                    Building(..),
                    Missile(..))
import Player
import Missile
import Building
import GameMap
import GameState
import Magic
import Coord

tickEngine :: GameState -> GameState
tickEngine = gainEnergy . collideMissiles . tickMissiles . tickBuildings

tickMissiles :: GameState -> GameState
tickMissiles = mapMyPlayer (moveMissiles MoveRight) . mapOponentsPlayer (moveMissiles MoveLeft)

moveMissiles :: Direction -> Player -> Player
moveMissiles direction player = mapMissiles (moveMissile direction) player

gainEnergy :: GameState -> GameState
gainEnergy =
  mapMyPlayer (incrementEnergy) . mapOponentsPlayer (incrementEnergy)

incrementEnergy :: Player -> Player
incrementEnergy player' =
  updateEnergy (energyPerTurn + energyFromTowers) player'
  where
    energyFromTowers = mapFold incrementEnergyAcc 0 $ towerMap player'

incrementEnergyAcc :: Building -> Int -> Int
incrementEnergyAcc (Building { buildingType = buildingType' }) energyAcc =
  if buildingType' == ENERGY
  then energyTowerEnergyGeneratedPerTurn + energyAcc
  else energyAcc

collideMissiles :: GameState -> GameState
collideMissiles state =
  state''
  where
    state'           = collideMissiles' oponentsMissiles me'      findRightOf updateOponentsMissiles updateMe      state
    state''          = collideMissiles' myMissiles       oponent' findLeftOf  updateMyMissiles       updateOponent state'
    me'              = me      state
    oponent'         = oponent state'
    oponentsMissiles = ownedMissiles (oponent state)
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
           Nothing         ->
             (didntCollide,
              decrementFitness y' building' $
              updateTowerMap (removeAt (toCoord xHit y') towerMap') player')
           Just building'' ->
             (didntCollide,
              decrementFitness y' building' $
              updateTowerMap (replaceAt building'' (toCoord xHit y') towerMap') player')
  where
    towerMap' = towerMap player'
