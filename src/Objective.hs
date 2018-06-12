module Objective (boardScore, Move(..))
  where

import Interpretor (GameState(..),
                    Missile(..),
                    GameDetails(..),
                    Command(..),
                    CellContents(..),
                    Building(..),
                    BuildingType(..),
                    BuildingPriceIndex(..),
                    PlayerType(..),
                    Player(..),
                    SparseMap)
import Engine
import GameMap
import Player
import Data.Maybe
import qualified Data.Vector as V

data Move = Move { myMove       :: Command,
                   oponentsMove :: Command }
          deriving (Show)

boardScore :: (GameState, Move) -> (Float, (GameState, Move))
boardScore withMove@(state, _) =
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
