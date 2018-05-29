module SearchSpace (myAvailableMoves,
                    oponentsAvailableMoves,
                    advanceState,
                    allCells,
                    cellIsEmpty,
                    search)
  where

import Interpretor (GameState(..),
                    Command(..),
                    PlayerType(..))
import System.Random
import Engine
import Cell
import Logic
import GameState as G
import Data.List as L
import Player
import Towers
import Data.Maybe

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

-- TODO dynamically scale the number of choices
-- TODO Iteratively search deeper
search :: RandomGen g => g -> GameState -> Maybe (Command, g)
search g state =
  Just (snd $ head choices, g')
  where
    (choices, g') = chooseN 1 g $ score $ advanceState state

advanceState :: GameState -> [(GameState, Command)]
advanceState state = do
  let newState = tickEngine state
  myMove       <- doNothingIfNoMoves $ myAvailableMoves state
  oponentsMove <- doNothingIfNoMoves $ oponentsAvailableMoves state
  return $ (newState `updateMyMove` myMove `updateOponentsMove` oponentsMove, myMove)

-- TODO Implement a better scoring function
score :: [(GameState, Command)] -> [(Float, (GameState, Command))]
score = zip [0..]

zipCDF :: [(GameState, Command)] -> [(Float, (GameState, Command))]
zipCDF xs =
  zip normalised $ sorted
  where
    normalised = map (/ (head summed)) summed
    summed     = (reverse . scanl1 (+) . map (fromIntegral . boardScore . fst)) sorted
    sorted     = sortOn (boardScore . fst) xs

eliteChoices :: Int
eliteChoices = 3

chooseN :: RandomGen g => Int -> g -> [(Float, (GameState, Command))] -> ([(GameState, Command)], g)
chooseN n g xs =
  (elite ++ randomChoices, g''')
  where
    (_, max')              = genRange g
    floatingMax            = fromIntegral max'
    normalise              = (/ floatingMax) . fromIntegral . abs
    elite                  = (map snd $ take eliteChoices xs)
    (randomChoices, g''')  = foldr choose ([], g) [1..n]
    choose _ (choices, g') =
      let (value, g'') = next g'
          normalised   = normalise value
          scanForValue = (snd . fromJust . find ((<= normalised) . fst))
      in (scanForValue xs : choices, g'')

