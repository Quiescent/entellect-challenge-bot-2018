{-# LANGUAGE BangPatterns #-}

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
import BuildingsUnderConstruction
import Coord

import Data.Int
import System.Clock
import Data.Maybe
import System.Random
import qualified Data.List as L

availableMoves :: (Coord -> Bool) -> Player -> [Command]
availableMoves constrainCellsTo player@(Player { towerMap = towerMap',
                                                 constructionQueue = constructionQueue' }) = do
  coord         <- filter (not . (flip definedAt) towerMap') $ filter (not . (flip containsSite constructionSites)) $ filter constrainCellsTo allCells
  buildingType' <- buildingsWhichICanAfford
  let (x, y) = fromCoord coord
  return $ Build x y buildingType'
  where
    constructionSites        = buildingConstructionSites constructionQueue'
    buildingsWhichICanAfford = map snd $ filter ((<= energy') . fst) prices
    energy'                  = energy player
    prices                   = towerPrices

myAvailableMoves :: GameState -> [Command]
myAvailableMoves state =
  NothingCommand : (availableMoves cellBelongsToMe $ me state)

oponentsAvailableMoves :: GameState -> [Command]
oponentsAvailableMoves state =
  NothingCommand : (availableMoves cellBelongsToOponent $ oponent state)

maxSearchTime :: Int64
maxSearchTime = 150000000

search :: RandomGen g => g -> GameState -> IO Command
search g state = do
  let clock = Realtime
  startTime <- getTime clock
  result    <- searchDeeper clock (nsec startTime) g'' selected
  return $ fst $ result
  where
    (selected, g'')      = chooseN breadthToSearch g' $ zipCDF $ map (myBoardScore) initialChoices
    (initialChoices, g') = advanceState g state

maximumByScore :: [(Float, (GameState, Move))] -> (Float, (GameState, Move))
maximumByScore = L.maximumBy ( \ (x, _) (y, _) -> compare x y )

-- TODO Remove finished ones from the search as we go
searchDeeper :: RandomGen g => Clock -> Int64 -> g -> [(Float, (GameState, Move))] -> IO (Command, g)
searchDeeper clock startTime g !states = do
  currentTime <- getTime clock
  if nsec currentTime - startTime > maxSearchTime
    then return (myMove $ snd $ snd $ maximumByScore states, g)
    else searchDeeper clock startTime g'' selected
  where
    (nextStates, g') = foldr ( \ (_, (state, move)) (statesAcc, g''') ->
                                 let (newStates, g'''') = advanceState g''' state
                                 in  (map ( \ (state', _) ->  (state', move)) newStates ++ statesAcc, g''''))
                             ([], g)
                             states
    (selected, g'')  = chooseN breadthToSearch g' $ zipCDF $ map (myBoardScore) nextStates

breadthToSearch :: Int
breadthToSearch = 5

advanceState :: RandomGen g => g -> GameState -> ([(GameState, Move)], g)
advanceState g gameState =
  (do
      myCommand       <- myAvailableMoves       gameState
      oponentsCommand <- oponentsAvailableMoves gameState
      return (updateMyMove myCommand $ updateOponentsMove oponentsCommand $ tickEngine gameState,
              Move myCommand oponentsCommand),
    g)

zipCDF :: Show a => [(Float, (GameState, a))] -> [(Float, (GameState, a))]
zipCDF xs =
  zipWith ( \ x (_, y) -> (x, y)) normalised (head descending : descending)
  where
    descending = reverse sorted
    normalised = map (/ (head summed)) summed
    summed     = (reverse . scanl (+) 0 . map fst) sorted
    sorted     = L.sortOn fst adjusted
    adjusted   = map (\ (boardScore, x) -> (minValue + boardScore, x)) xs
    minValue   = abs $ minimum $ map fst xs

eliteChoices :: Int
eliteChoices = 3

chooseN :: RandomGen g => Int -> g -> [(Float, (GameState, a))] -> ([(Float, (GameState, a))], g)
chooseN n g xs =
  (elite ++ randomChoices, g''')
  where
    (_, max')              = genRange g
    floatingMax            = fromIntegral max'
    normalise              = (/ floatingMax) . fromIntegral . abs
    elite                  = take eliteChoices xs
    (randomChoices, g''')  = foldr choose ([], g) [1..(n - eliteChoices - 1)]
    choose _ (choices, g') =
      let (value, g'') = next g'
          normalised   = normalise value
          scanForValue = fromJust . lastIfNothing xs . L.find ((<= normalised) . fst)
      in (scanForValue xs : choices, g'')

lastIfNothing :: [(Float, (GameState, a))] -> Maybe (Float, (GameState, a)) -> Maybe (Float, (GameState, a))
lastIfNothing _  x@(Just _) = x
lastIfNothing xs Nothing    = Just $ last xs
