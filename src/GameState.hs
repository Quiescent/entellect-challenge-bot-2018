module GameState (runCommand,
                  mapMyPlayer,
                  mapOponentsPlayer,
                  UpdateMissiles,
                  updateMyMissiles,
                  updateOponentsMissiles,
                  UpdatePlayer,
                  mapMyMap,
                  mapOponentsMap,
                  updateMe,
                  updateOponent,
                  incrementMyHitsTaken,
                  incrementOponentsHitsTaken,
                  buildForMe,
                  buildForOponent,
                  Command(..),
                  updateMyMove,
                  updateOponentsMove)
  where

import Interpretor (GameState(..),
                    Building(..),
                    Missile(..),
                    TowerStats(..),
                    Command(..),
                    Player(..),
                    TowerMap,
                    GameDetails(..))
import Player
import GameDetails
import GameMap
import BuildingsUnderConstruction

type MapPlayer = (Player -> Player) -> GameState -> GameState

incrementMyHitsTaken :: GameState -> GameState
incrementMyHitsTaken = mapMyPlayer incrementHitsTaken

incrementOponentsHitsTaken :: GameState -> GameState
incrementOponentsHitsTaken = mapOponentsPlayer incrementHitsTaken

mapMyPlayer :: MapPlayer
mapMyPlayer f state@(GameState { me = me' }) =
  state { me = f me' }

mapOponentsPlayer :: MapPlayer
mapOponentsPlayer f state@(GameState { oponent = oponent' }) =
  state { oponent = f oponent' }

runCommand :: GameDetails -> Player -> Command -> Player
runCommand _       player NothingCommand              = player
runCommand _       player (Deconstruct x' y')         =
  mapMap (removeAt (x', y')) player
runCommand details player (Build x' y' buildingType') = 
  player { constructionQueue = addBuilding (createBuildingUnderConstruction constructionTime' x' y' building')
                                           (constructionQueue player) }
  where
    constructionTime' = constructionTime buildingStats'
    building'         = buildingFromStats buildingType' buildingStats'
    buildingStats'    = towerStats buildingType' details

type UpdateMissiles = [Missile] -> GameState -> GameState

updateMyMissiles :: UpdateMissiles
updateMyMissiles missiles = mapMyPlayer (updateMissiles missiles)

updateOponentsMissiles :: UpdateMissiles
updateOponentsMissiles missiles = mapOponentsPlayer (updateMissiles missiles)

type UpdatePlayer = Player -> GameState -> GameState

updateMe :: UpdatePlayer
updateMe player' = mapMyPlayer (\ _ -> player')

updateOponent :: UpdatePlayer
updateOponent player' = mapOponentsPlayer (\ _ -> player')

mapMyMap :: (TowerMap -> TowerMap) -> GameState -> GameState
mapMyMap f = mapMyPlayer (mapMap f)

mapOponentsMap :: (TowerMap -> TowerMap) -> GameState -> GameState
mapOponentsMap f = mapOponentsPlayer (mapMap f)

updateMyMove :: GameDetails -> Command -> GameState -> GameState
updateMyMove details command = mapMyPlayer (updateMove details command)

updateOponentsMove :: GameDetails -> Command -> GameState -> GameState
updateOponentsMove details command = mapOponentsPlayer (updateMove details command)

buildForMe :: Int -> Int -> Int -> Building -> GameState -> GameState
buildForMe timeLeft x' y' building' = mapMyPlayer (build timeLeft x' y' building')

buildForOponent :: Int -> Int -> Int -> Building -> GameState -> GameState
buildForOponent timeLeft x' y' building' = mapMyPlayer (build timeLeft x' y' building')
