module SearchSpace (advanceState,
                    search)
  where

import Interpretor (GameState(..),
                    Player(..),
                    Command(..),
                    GameDetails(..))
import Engine
import GameMap
import Cell
import GameState
import Towers
import Objective

import Data.Maybe
import System.Random
import qualified Data.List as L

availableMoves :: GameDetails -> ((Int, Int) -> Bool) -> Player -> [Command]
availableMoves details constrainCellsTo player@(Player { towerMap = towerMap' }) = do
  (x, y) <- filter constrainCellsTo allCells
  if definedAt (x, y) towerMap'
    then
    do
      buildingType' <- buildingsWhichICanAfford
      return $ Build x y buildingType'
    else return $ Deconstruct x y
  where
    buildingsWhichICanAfford = map snd $ filter ((<= energy') . fst) prices
    energy'                  = energy player
    prices                   = towerPrices details

myAvailableMoves :: GameDetails -> GameState -> [Command]
myAvailableMoves details state =
  NothingCommand : (availableMoves details cellBelongsToMe $ me state)

oponentsAvailableMoves :: GameDetails -> GameState -> [Command]
oponentsAvailableMoves details state =
  NothingCommand : (availableMoves details cellBelongsToOponent $ oponent state)

doNothingIfNoMoves :: [Command] -> [Command]
doNothingIfNoMoves [] = [NothingCommand]
doNothingIfNoMoves xs = xs

search :: RandomGen g => g -> GameDetails -> GameState -> Maybe (Command, g)
search g details state =
  Just (searchDeeper g' details depthToSearch initialChoices)
  where
    (initialChoices, g') = advanceState g details state

maximumByScore :: [(Float, (GameState, Move))] -> (Float, (GameState, Move))
maximumByScore = L.maximumBy ( \ (x, _) (y, _) -> compare x y )

-- TODO Remove finished ones from the search as we go
-- TODO Time box this
searchDeeper :: RandomGen g => g -> GameDetails -> Int -> [(GameState, Move)] -> (Command, g)
searchDeeper g details 0         states = (myMove $ snd $ snd $ maximumByScore $ map (myBoardScore details) states, g)
searchDeeper g details remaining states =
  searchDeeper g'' details (remaining - 1) selected
  where
    (nextStates, g') = foldr ( \ (state, move) (statesAcc, g''') ->
                                 let (newStates, g'''') = advanceState g''' details state
                                 in  (map ( \ (state', _) ->  (state', move)) newStates ++ statesAcc, g''''))
                             ([], g)
                             states
    (selected, g'')  = chooseN breadthToSearch g' $ zipCDF $ map (myBoardScore details) nextStates

breadthToSearch :: Int
breadthToSearch = 12

depthToSearch :: Int
depthToSearch = 40

splay :: Int
splay = 5

advanceState :: RandomGen g => g -> GameDetails -> GameState -> ([(GameState, Move)], g)
advanceState g details gameState =
  (do
      myCommand       <- map snd myStates
      oponentsCommand <- map snd oponentsStates
      return (updateMyMove details myCommand $ updateOponentsMove details oponentsCommand $ tickEngine details gameState,
              Move myCommand oponentsCommand),
    g'')
  where
    chooseCandidates g''' = chooseN splay g''' . zipCDF
    (myStates, g')        =
      chooseCandidates g $
      map (myBoardScore details) $
      myMoves details gameState
    (oponentsStates, g'') =
      chooseCandidates g' $
      invertScores $
      map (myBoardScore details) $
      oponentsMoves details gameState

invertScores :: [(Float, (GameState, Command))] -> [(Float, (GameState, Command))]
invertScores = map ( \ (score', x) -> (1.0 / score', x))

myMoves :: GameDetails -> GameState -> [(GameState, Command)]
myMoves details state = do
  myMove'       <- doNothingIfNoMoves $ myAvailableMoves details state
  return $ (updateMyMove details myMove' state, myMove')

oponentsMoves :: GameDetails -> GameState -> [(GameState, Command)]
oponentsMoves details state = do
  oponentsMove' <- doNothingIfNoMoves $ oponentsAvailableMoves details state
  return $ (updateOponentsMove details oponentsMove' state, oponentsMove')

zipCDF :: [(Float, (GameState, a))] -> [(Float, (GameState, a))]
zipCDF xs =
  zipWith ( \ x (_, y) -> (x, y)) normalised sorted
  where
    normalised = map (/ (head summed)) summed
    summed     = (reverse . scanl1 (+) . map fst) sorted
    sorted     = L.sortOn fst xs

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
