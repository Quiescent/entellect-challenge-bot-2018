module Player (updateEnergy, myPlayer, oponentsPlayer, myEnergy, oponentsEnergy, incrementMyHitsTaken, incrementOponentsHitsTaken)
  where

import Interpretor (GameState(..),
                    Player(..),
                    PlayerType(..))
import Data.Vector as V
import Data.Maybe
import Prelude as P

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

mapPlayer :: (GameState -> Player) -> (Player -> Player) -> GameState -> GameState
mapPlayer player f state =
  let players'  = players state
      player' = player state
  in state { players = V.filter (== player') players' `V.snoc` (f player') }

mapMyPlayer :: (Player -> Player) -> GameState -> GameState
mapMyPlayer = mapPlayer myPlayer

mapOponentsPlayer :: (Player -> Player) -> GameState -> GameState
mapOponentsPlayer = mapPlayer oponentsPlayer

incrementHitsTaken :: ((Player -> Player) -> GameState -> GameState) -> GameState -> GameState
incrementHitsTaken mapPlayer' =
  mapPlayer' ( \ player'@(Player { hitsTaken = hitsTaken' }) -> player' { hitsTaken = hitsTaken' + 1 })

incrementMyHitsTaken :: GameState -> GameState
incrementMyHitsTaken = incrementHitsTaken mapMyPlayer

incrementOponentsHitsTaken :: GameState -> GameState
incrementOponentsHitsTaken = incrementHitsTaken mapOponentsPlayer
