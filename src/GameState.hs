module GameState (runCommand,
                  mapMyPlayer,
                  mapOponentsPlayer,
                  UpdatePlayer,
                  updateMe,
                  updateOponent,
                  Command(..),
                  updateMyMove,
                  updateOponentsMove,
                  collideAndMoveMissiles)
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

-- Order is: collide, check boundary conditions, move.  This is
-- repeated twice and then the missiles are finally collided again.
collideAndMoveMissiles :: GameState -> GameState
collideAndMoveMissiles 
  state@(GameState { me = me',
                     oponent = oponent'}) =
  let (me1, oponent1)   = collide me' oponent'
      (oponent2, me2)   = collide oponent1 me1
      (me3, oponent3)   = moveCheckingBoundaries me2 oponent2
      (oponent4, me4)   = moveCheckingBoundaries oponent3 me3
      (me5, oponent5)   = collide me4 oponent4
      (oponent6, me6)   = collide oponent5 me5
      (me7, oponent7)   = moveCheckingBoundaries me6 oponent6
      (oponent8, me8)   = moveCheckingBoundaries oponent7 me7
      (me9, oponent9)   = collide me8 oponent8
      (oponent10, me10) = collide oponent9 me9
  in state { me      = me10,
             oponent = oponent10 }

type UpdatePlayer = Player -> GameState -> GameState

updateMe :: UpdatePlayer
updateMe player' = mapMyPlayer (\ _ -> player')

updateOponent :: UpdatePlayer
updateOponent player' = mapOponentsPlayer (\ _ -> player')

updateMyMove :: EfficientCommand -> GameState -> GameState
updateMyMove command = mapMyPlayer (updateMove command)

updateOponentsMove :: EfficientCommand -> GameState -> GameState
updateOponentsMove command = mapOponentsPlayer (updateMove command)
