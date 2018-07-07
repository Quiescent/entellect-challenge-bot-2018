module SearchSpace (advanceState,
                    myAvailableMoves,
                    oponentsAvailableMoves,
                    search)
  where

import Interpretor (GameState(..),
                    Player(..),
                    Command(..))
import Engine
import GameMap
import Cell
import GameState
import Towers
import Objective

import Data.Maybe
import System.Random
import qualified Data.List as L

availableMoves :: ((Int, Int) -> Bool) -> Player -> [Command]
availableMoves constrainCellsTo player@(Player { towerMap = towerMap' }) = do
  (x, y) <- filter constrainCellsTo allCells
  if not $ definedAt (x, y) towerMap'
    then do
      buildingType' <- buildingsWhichICanAfford
      return $ Build x y buildingType'
  else return $ Deconstruct x y
  where
    buildingsWhichICanAfford = map snd $ filter ((<= energy') . fst) prices
    energy'                  = energy player
    prices                   = towerPrices

myAvailableMoves :: GameState -> [Command]
myAvailableMoves state =
  NothingCommand : (availableMoves cellBelongsToMe $ me state)

oponentsAvailableMoves :: GameState -> [Command]
oponentsAvailableMoves state =
  NothingCommand : (availableMoves cellBelongsToOponent $ oponent state)

doNothingIfNoMoves :: [Command] -> [Command]
doNothingIfNoMoves [] = [NothingCommand]
doNothingIfNoMoves xs = xs

search :: RandomGen g => g -> GameState -> Maybe (Command, g)
search g state =
  Just (searchDeeper g' depthToSearch initialChoices)
  where
    (initialChoices, g') = advanceState g state

maximumByScore :: [(Float, (GameState, Move))] -> (Float, (GameState, Move))
maximumByScore = L.maximumBy ( \ (x, _) (y, _) -> compare x y )

-- TODO Remove finished ones from the search as we go
-- TODO Time box this
searchDeeper :: RandomGen g => g -> Int -> [(GameState, Move)] -> (Command, g)
searchDeeper g 0         states = (myMove $ snd $ snd $ maximumByScore $ map (myBoardScore) states, g)
searchDeeper g remaining states =
  searchDeeper g'' (remaining - 1) selected
  where
    (nextStates, g') = foldr ( \ (state, move) (statesAcc, g''') ->
                                 let (newStates, g'''') = advanceState g''' state
                                 in  (map ( \ (state', _) ->  (state', move)) newStates ++ statesAcc, g''''))
                             ([], g)
                             states
    (selected, g'')  = chooseN breadthToSearch g' $ zipCDF $ map (myBoardScore) nextStates

breadthToSearch :: Int
breadthToSearch = 12

depthToSearch :: Int
depthToSearch = 30

splay :: Int
splay = 5

advanceState :: RandomGen g => g -> GameState -> ([(GameState, Move)], g)
advanceState g gameState =
  (do
      myCommand       <- map snd myStates
      oponentsCommand <- map snd oponentsStates
      return (updateMyMove myCommand $ updateOponentsMove oponentsCommand $ tickEngine gameState,
              Move myCommand oponentsCommand),
    g'')
  where
    chooseCandidates g''' = chooseN splay g''' . zipCDF
    (myStates, g')        =
      chooseCandidates g $
      map (myBoardScore) $
      myMoves gameState
    (oponentsStates, g'') =
      chooseCandidates g' $
      invertScores $
      map (myBoardScore) $
      oponentsMoves gameState

invertScores :: [(Float, (GameState, Command))] -> [(Float, (GameState, Command))]
invertScores = map ( \ (score', x) -> (1.0 / score', x))

myMoves :: GameState -> [(GameState, Command)]
myMoves state = do
  myMove'       <- doNothingIfNoMoves $ myAvailableMoves state
  return $ (updateMyMove myMove' state, myMove')

oponentsMoves :: GameState -> [(GameState, Command)]
oponentsMoves state = do
  oponentsMove' <- doNothingIfNoMoves $ oponentsAvailableMoves state
  return $ (updateOponentsMove oponentsMove' state, oponentsMove')

zipCDF :: [(Float, (GameState, a))] -> [(Float, (GameState, a))]
zipCDF xs =
  zipWith ( \ x (_, y) -> (x, y)) normalised sorted
  where
    normalised = map (/ (head summed)) summed
    summed     = (reverse . scanl1 (+) . map fst) sorted
    sorted     = L.sortOn fst adjusted
    adjusted   = map (\ (boardScore, x) -> (minValue + boardScore, x)) xs
    minValue   = minimum $ map fst xs

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
          scanForValue = (snd . fromJust . lastIfNothing xs . L.find ((<= normalised) . fst))
      in (scanForValue xs : choices, g'')

lastIfNothing :: [(Float, (GameState, a))] -> Maybe (Float, (GameState, a)) -> Maybe (Float, (GameState, a))
lastIfNothing _  x@(Just _) = x
lastIfNothing xs Nothing    = Just $ last xs
