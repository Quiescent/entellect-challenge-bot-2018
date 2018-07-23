module Engine (tickEngine)
  where

import Interpretor (decrementFitness,
                    GameState(..),
                    BuildingType(..),
                    Player(..),
                    Building(..),
                    Missile,
                    Missiles)
import Player
import Missile
import Building
import GameMap
import GameState
import Magic
import Coord

import Control.Monad.State.Lazy

tickEngine :: GameState -> GameState
tickEngine = gainEnergy . collideMissiles . tickMissiles . tickBuildings

tickMissiles :: GameState -> GameState
tickMissiles = mapMyPlayer (moveMissiles MoveRight) . mapOponentsPlayer (moveMissiles MoveLeft)

moveMissiles :: Direction -> Player -> Player
moveMissiles direction player = mapMissiles (moveMissile direction) player

gainEnergy :: GameState -> GameState
gainEnergy = mapMyPlayer (incrementEnergy) . mapOponentsPlayer (incrementEnergy)

incrementEnergy :: Player -> Player
incrementEnergy player' = updateEnergy (energyPerTurn + (energyGenPerTurn player')) player'

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

collideMissiles' :: Missiles -> Player -> CollisionDetector -> UpdateMissiles -> UpdatePlayer -> GameState -> GameState
collideMissiles' missiles player' collisionDetector updateMissiles' updatePlayer =
  updateMissiles' missilesRemaining . updatePlayer updatedPlayer
  where
    (missilesRemaining, updatedPlayer) = runState (filterMMissiles (collideMissile collisionDetector) missiles) player'

collideMissile :: CollisionDetector -> Missile -> State Player Bool
collideMissile collisionDetector missile = do
  player'      <- get
  let towerMap' = towerMap player'
  case collisionDetector missile towerMap' of
    HitNothing                 -> return True
    HitPlayer                  -> do
      put $ takeDamage missileDamage player'
      return False
    HitBuilding coordHit building' ->
      let damaged = missileDamagesBuilding building'
      in case damaged of
           Nothing         -> do
             put $ decrementFitness (getY coordHit) building' $
                   updateTowerMap (removeAt coordHit towerMap') player'
             return False
           Just building'' -> do
             put $ decrementFitness (getY coordHit) building' $
                   updateTowerMap (replaceAt building'' coordHit towerMap') player'
             return False
