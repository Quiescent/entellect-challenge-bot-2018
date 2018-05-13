module Player (ourPlayer, ourEnergy)
  where

import Interpretor (GameState(..),
                    Player(..),
                    PlayerType(..))
import Data.Vector as V
import Data.Maybe

ourPlayer :: GameState -> Player
ourPlayer = (fromJust . V.find ((==A) . playerType) . players)

ourEnergy :: GameState -> Int
ourEnergy = energy . ourPlayer
