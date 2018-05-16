module SearchSpace (myAvailableMoves,
                    oponentsAvailableMoves,
                    advanceState,
                    allCells,
                    cellIsEmpty)
  where

import Interpretor (GameState(..),
                    Command(..))
import Engine
import Cell
import Logic
import GameState as G
import Data.List as L
import Player
import Towers

-- Notes: the bot rules aren't very clear on the rules surrounding
-- buildings in a block.  Their own example bot leads one to believe
-- that you can have multiple buildings in a cell at a time; however,
-- the rules state that you can't build in a square which already has
-- a building in it.  This leads me to believe that buildings might
-- end up having add-ons.

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

advanceState :: GameState -> [GameState]
advanceState state = do
  let newMap   = tickEngine state
  myMove       <- myAvailableMoves state
  oponentsMove <- oponentsAvailableMoves state
  -- NOTE: Possible optimisation: do missiles, then my move as
  -- intermediaries
  return $ newMap `G.update` myMove `G.update` oponentsMove
