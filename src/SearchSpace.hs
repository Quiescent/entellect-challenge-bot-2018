module SearchSpace (ourAvailableMoves, oponentsAvailableMoves)
  where

import Interpretor (GameState(..),
                    Command,
                    CellStateContainer(..))
import Cell
import Command
import Data.Vector as V
import Data.List as L
import Player
import Towers

-- Notes: the bot rules aren't very clear on the rules surrounding
-- buildings in a block.  Their own example bot leads one to believe
-- that you can have multiple buildings in a cell at a time; however,
-- the rules state that you can't build in a square which already has
-- a building in it.  This leads me to believe that buildings might
-- end up having add-ons.

availableMoves :: (CellStateContainer -> Bool) -> (GameState -> Int) -> GameState -> [Command]
availableMoves constrainCellsTo playerEnergy state@(GameState {gameMap = mapGrid}) = do
  row      <- toList mapGrid
  openCell <- L.filter constrainCellsTo $ toList row
  building <- buildingsThatCanBeBuilt openCell
  return $ build (xPos openCell) (yPos openCell) building
  where
    buildingsThatCanBeBuilt (CellStateContainer { buildings = buildings' })
      | buildings' == V.empty = buildingsWhichICanAfford
      | otherwise             = []
    buildingsWhichICanAfford  = L.map snd $ L.filter ((<= energy') . fst) prices
    energy'                   = playerEnergy state
    prices                    = towerPrices $ gameDetails state

ourAvailableMoves :: GameState -> [Command]
ourAvailableMoves = availableMoves cellBelongsToMe ourEnergy

oponentsAvailableMoves :: GameState -> [Command]
oponentsAvailableMoves = availableMoves cellBelongsToOponent oponentsEnergy
