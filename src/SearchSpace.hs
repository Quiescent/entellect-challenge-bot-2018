module SearchSpace (myAvailableMoves,
                    oponentsAvailableMoves,
                    advanceState,
                    allCells,
                    cellContainsNoBuildings,
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
import Objective
import Data.Maybe

availableMoves :: ((Int, Int) -> Bool) -> (GameState -> Int) -> GameState -> [Command]
availableMoves constrainCellsTo playerEnergy state@(GameState {gameMap = mapGrid}) = do
  (x, y)    <- L.filter (cellContainsNoBuildings mapGrid &&& constrainCellsTo) $ allCells state
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

search :: RandomGen g => g -> GameState -> Maybe (Command, g)
search g state =
  Just (searchDeeper g' depthToSearch initialChoices)
  where
    (initialChoices, g') = advanceState g state

searchDeeper :: RandomGen g => g -> Int -> [(GameState, Move)] -> (Command, g)
searchDeeper g 0         states = (myMove $ snd $ head states, g)
searchDeeper g remaining states =
  searchDeeper g'' (remaining - 1) selected
  where
    (nextStates, g') = foldr ( \ (state, move) (statesAcc, g''') ->
                                 let (newStates, g'''') = advanceState g''' state
                                 in  (map ( \ (state', _) ->  (state', move)) newStates ++ statesAcc, g''''))
                             ([], g)
                             states
    (selected, g'')  = chooseN breadthToSearch g' $ zipCDF $ map myBoardScore nextStates

breadthToSearch :: Int
breadthToSearch = 20

depthToSearch :: Int
depthToSearch = 100

splay :: Int
splay = 5

advanceState :: RandomGen g => g -> GameState -> ([(GameState, Move)], g)
advanceState g gameState =
  (do
      myCommand       <- map snd myStates
      oponentsCommand <- map snd oponentsStates
      return ((tickEngine gameState) `updateMyMove` myCommand `updateOponentsMove` oponentsCommand,
              Move myCommand oponentsCommand),
    g'')
  where
    chooseCandidates g''' = chooseN splay g''' . zipCDF
    (myStates, g')        = chooseCandidates g  $ map myBoardScore $ myMoves       gameState
    (oponentsStates, g'') = chooseCandidates g' $ invertScores $ map myBoardScore $ oponentsMoves gameState

invertScores :: [(Float, (GameState, Command))] -> [(Float, (GameState, Command))]
invertScores = map ( \ (score, x) -> (1.0 / score, x))

myMoves :: GameState -> [(GameState, Command)]
myMoves state = do
  myMove'       <- doNothingIfNoMoves $ myAvailableMoves state
  return $ (state `updateMyMove` myMove', myMove')

oponentsMoves :: GameState -> [(GameState, Command)]
oponentsMoves state = do
  oponentsMove' <- doNothingIfNoMoves $ oponentsAvailableMoves state
  return $ (state `updateOponentsMove` oponentsMove', oponentsMove')

zipCDF :: [(Float, (GameState, a))] -> [(Float, (GameState, a))]
zipCDF xs =
  zipWith ( \ x (_, y) -> (x, y)) normalised sorted
  where
    normalised = map (/ (head summed)) summed
    summed     = (reverse . scanl1 (+) . map fst) sorted
    sorted     = sortOn fst xs

eliteChoices :: Int
eliteChoices = 3

chooseN :: RandomGen g => Int -> g -> [(Float, (GameState, a))] -> ([(GameState, a)], g)
chooseN n g xs =
  (elite ++ randomChoices, g''')
  where
    (_, max')              = genRange g
    floatingMax            = fromIntegral max'
    normalise              = (/ floatingMax) . fromIntegral . abs
    elite                  = (map snd $ take eliteChoices xs)
    (randomChoices, g''')  = foldr choose ([], g) [1..(n - eliteChoices)]
    choose _ (choices, g') =
      let (value, g'') = next g'
          normalised   = normalise value
          scanForValue = (snd . fromJust . lastIfNothing xs . find ((<= normalised) . fst))
      in (scanForValue xs : choices, g'')

lastIfNothing :: [(Float, (GameState, a))] -> Maybe (Float, (GameState, a)) -> Maybe (Float, (GameState, a))
lastIfNothing _  x@(Just _) = x
lastIfNothing xs Nothing    = Just $ last xs
