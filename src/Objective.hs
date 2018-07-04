module Objective (myBoardScore, Move(..))
  where

import Interpretor (GameState(..),
                    Command(..))
import Player
import Engine
import Magic

data Move = Move { myMove       :: Command,
                   oponentsMove :: Command }
          deriving (Show)

myBoardScore :: (GameState, a) -> (Float, (GameState, a))
myBoardScore withMove@(state, _) =
  (fromIntegral $ damageToOponent - damageToMe, withMove)
  where
    myInitialHealth       = myHealth state
    myFinalHealth         = myHealth state'
    damageToMe            = myInitialHealth - myFinalHealth
    oponentsFinalHealth   = oponentsHealth state'
    oponentsInitialHealth = oponentsHealth state
    damageToOponent       = oponentsInitialHealth - oponentsFinalHealth
    state' = (!! turnsIntoFuture) $ iterate tickEngine state

turnsIntoFuture :: Int
turnsIntoFuture = 5
