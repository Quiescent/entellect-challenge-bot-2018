module SearchSpace
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

availableMoves :: GameState -> [Command]
availableMoves state@(GameState {gameMap = mapGrid}) = do
  row      <- toList mapGrid
  openCell <- L.filter cellBelongsToMe $ toList row
  building <- buildingsThatCanBeBuilt openCell
  return $ build (xPos openCell) (yPos openCell) building
  where
    -- TODO: When is a cell full of buildings?
    -- NOTE: Takes a cell as input
    buildingsThatCanBeBuilt _ = buildingsWhichICanAfford
    buildingsWhichICanAfford  = L.map snd $ L.filter ((<= energy') . fst) prices
    energy'                   = ourEnergy state
    prices                    = towerPrices $ gameDetails state
