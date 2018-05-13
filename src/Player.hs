module Player (ourPlayer, oponentsPlayer, ourEnergy, oponentsEnergy)
  where

import Interpretor (GameState(..),
                    Player(..),
                    PlayerType(..))
import Data.Vector as V
import Data.Maybe

player :: PlayerType -> GameState -> Player
player playerType' = (fromJust . V.find ((==playerType') . playerType) . players)

ourPlayer :: GameState -> Player
ourPlayer = player A

oponentsPlayer :: GameState -> Player
oponentsPlayer = player B

playerEnergy :: (GameState -> Player) -> GameState -> Int
playerEnergy player' = energy . player'

ourEnergy :: GameState -> Int
ourEnergy = playerEnergy ourPlayer

oponentsEnergy :: GameState -> Int
oponentsEnergy = playerEnergy oponentsPlayer
