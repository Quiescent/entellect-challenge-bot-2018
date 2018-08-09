module GameState (runCommand,
                  mapMyPlayer,
                  mapOponentsPlayer,
                  UpdatePlayer,
                  updateMe,
                  updateOponent,
                  Command(..),
                  updateMyMove,
                  updateOponentsMove)
  where

import Interpretor (GameState(..), Command(..), Player(..))

import Player
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
  deconstructAt coord' player
runCommand player (Build coord' buildingType') =
  buildOnMap coord' building' player
  where
    building' = buildingFromStats buildingType'

type UpdatePlayer = Player -> GameState -> GameState

updateMe :: UpdatePlayer
updateMe player' = mapMyPlayer (\ _ -> player')

updateOponent :: UpdatePlayer
updateOponent player' = mapOponentsPlayer (\ _ -> player')

updateMyMove :: EfficientCommand -> GameState -> GameState
updateMyMove command = mapMyPlayer (updateMove command)

updateOponentsMove :: EfficientCommand -> GameState -> GameState
updateOponentsMove command = mapOponentsPlayer (updateMove command)
