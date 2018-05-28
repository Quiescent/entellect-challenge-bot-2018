module SearchSpace (myAvailableMoves,
                    oponentsAvailableMoves,
                    advanceState,
                    allCells,
                    cellIsEmpty)
  where

import Interpretor (GameState(..),
                    Command(..),
                    PlayerType(..))
import Engine
import Cell
import Logic
import GameState as G
import Data.List as L
import Player
import Towers

availableMoves :: ((Int, Int) -> Bool) -> (GameState -> Int) -> GameState -> [Command]
availableMoves constrainCellsTo playerEnergy state@(GameState {gameMap = mapGrid}) = do
  (x, y)    <- L.filter (cellIsEmpty mapGrid &&& constrainCellsTo) $ allCells state
  building' <- buildingsWhichICanAfford
  return $ Command x y building'
  where
    buildingsWhichICanAfford = L.map snd $ L.filter ((<= energy') . fst) prices
    energy'                  = playerEnergy state
    prices                   = towerPrices $ gameDetails state

myAvailableMoves :: GameState -> [Command]
myAvailableMoves state = availableMoves (cellBelongsToMe state) myEnergy state

oponentsAvailableMoves :: GameState -> [Command]
oponentsAvailableMoves state = availableMoves (cellBelongsToOponent state) oponentsEnergy state

updateMyMove :: GameState -> Command -> GameState
updateMyMove = G.update A

updateOponentsMove :: GameState -> Command -> GameState
updateOponentsMove = G.update B

doNothingIfNoMoves :: [Command] -> [Command]
doNothingIfNoMoves [] = [NothingCommand]
doNothingIfNoMoves xs = xs

advanceState :: GameState -> [(GameState, Command)]
advanceState state = do
  let newState   = tickEngine state
  myMove       <- doNothingIfNoMoves $ myAvailableMoves state
  oponentsMove <- doNothingIfNoMoves $ oponentsAvailableMoves state
  return $ (newState `updateMyMove` myMove `updateOponentsMove` oponentsMove, myMove)
