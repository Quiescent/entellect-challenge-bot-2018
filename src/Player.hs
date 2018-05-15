module Player (updateEnergy, myPlayer, oponentsPlayer, myEnergy, oponentsEnergy)
  where

import Interpretor (GameState(..),
                    Player(..),
                    PlayerType(..))
import Data.Vector as V
import Data.Maybe

player :: PlayerType -> GameState -> Player
player playerType' = (fromJust . V.find ((==playerType') . playerType) . players)

myPlayer :: GameState -> Player
myPlayer = player A

oponentsPlayer :: GameState -> Player
oponentsPlayer = player B

playerEnergy :: (GameState -> Player) -> GameState -> Int
playerEnergy player' = energy . player'

myEnergy :: GameState -> Int
myEnergy = playerEnergy myPlayer

oponentsEnergy :: GameState -> Int
oponentsEnergy = playerEnergy oponentsPlayer

updateEnergy :: GameState -> (Int, Int) -> GameState
updateEnergy state (myEnergy', oponentsEnergy') =
  let myPlayer'     = myPlayer state
      oponentPlayer = oponentsPlayer state
  in state { players = V.fromList [myPlayer'     { energy = myEnergy' },
                                   oponentPlayer { energy = oponentsEnergy' }] }
