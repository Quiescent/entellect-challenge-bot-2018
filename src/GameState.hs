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
                  Command(..),
                  updateMyMove,
                  updateOponentsMove)
  where

import Interpretor (GameState(..),
                    Command(..),
                    Player(..),
                    TowerMap,
                    Missile,
                    Missiles)
import Player
import GameMap
import BuildingsUnderConstruction
import Coord
import EfficientCommand

type MapPlayer = (Player -> Player) -> GameState -> GameState

mapMyPlayer :: MapPlayer
mapMyPlayer f state@(GameState { me = me' }) =
  state { me = f me' }

mapOponentsPlayer :: MapPlayer
mapOponentsPlayer f state@(GameState { oponent = oponent' }) =
  state { oponent = f oponent' }

runCommand :: Player -> Command -> Player
runCommand player NothingCommand               = player
runCommand player (Deconstruct coord')         =
  mapMap (removeAt coord') player
runCommand player (Build coord' buildingType') =
  player { constructionQueue = addBuilding (createBuildingUnderConstruction constructionTime' coord' building')
                                           (constructionQueue player) }
  where
    constructionTime' = constructionTime  buildingType'
    building'         = buildingFromStats buildingType'

type UpdateMissiles = Missiles -> GameState -> GameState

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

updateMyMove :: EfficientCommand -> GameState -> GameState
updateMyMove command = mapMyPlayer (updateMove command)

updateOponentsMove :: EfficientCommand -> GameState -> GameState
updateOponentsMove command = mapOponentsPlayer (updateMove command)
