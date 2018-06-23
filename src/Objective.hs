module Objective (myBoardScore, Move(..))
  where

import Interpretor (GameState(..),
                    GameDetails(..),
                    Command(..))
import Player
import Engine

data Move = Move { myMove       :: Command,
                   oponentsMove :: Command }
          deriving (Show)

myBoardScore :: GameDetails -> (GameState, a) -> (Float, (GameState, a))
myBoardScore details withMove@(state, _) =
  (fromIntegral $ damageToOponent - damageToMe, withMove)
  where
    myInitialHealth       = myHealth state
    myFinalHealth         = myHealth state'
    damageToMe            = myInitialHealth - myFinalHealth
    oponentsFinalHealth   = oponentsHealth state'
    oponentsInitialHealth = oponentsHealth state
    damageToOponent       = oponentsInitialHealth - oponentsFinalHealth
    state' = (!! turnsIntoFuture) $ iterate (tickEngine details) state

turnsIntoFuture :: Int
turnsIntoFuture = 5
