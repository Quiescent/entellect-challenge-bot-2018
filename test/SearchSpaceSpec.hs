module SearchSpaceSpec where

import SearchSpace
import Interpretor

import Test.Hspec

import qualified Data.IntMap     as M
import qualified Data.PQueue.Min as PQ
import qualified Data.Set        as S

spec :: Spec
spec = myAvailableMovesSpec >> oponentsAvailableMovesSpec

myAvailableMovesSpec :: Spec
myAvailableMovesSpec = do
  describe "myAvailableMoves" $ do
    it "should produce only the NothingCommand move and all possible Deconstructs when the map is full" $
      (S.fromList $ myAvailableMoves fullBoard) `shouldBe` S.fromList (NothingCommand : myDeconstructs)
    it "should produce only build commands and NothingCommand when the map is empty" $
      (S.fromList $ myAvailableMoves emptyBoard) `shouldBe` S.fromList (NothingCommand : myBuilds)
    it "should not produce a build command on an occupied square when there's only one square" $
      (S.fromList $ myAvailableMoves emptyBoardWithBuildingAtSevenSeven)
       `shouldBe`
       S.fromList (NothingCommand : myBuildsWithoutSevenSeven)

oponentsAvailableMovesSpec :: Spec
oponentsAvailableMovesSpec = do
  describe "oponentsAvailableMoves" $ do
    it "should produce only the NothingCommand move and all possible Deconstructs when the map is full" $
      (S.fromList $ oponentsAvailableMoves fullBoard) `shouldBe` S.fromList (NothingCommand : oponentsDeconstructs)
    it "should produce only build commands and NothingCommand when the map is empty" $
      (S.fromList $ oponentsAvailableMoves emptyBoard) `shouldBe` S.fromList (NothingCommand : oponentsBuilds)
    it "should not produce a build command on an occupied square when there's only one square" $
      (S.fromList $ oponentsAvailableMoves emptyBoardWithBuildingAtSevenSeven)
       `shouldBe`
       S.fromList (NothingCommand : oponentsBuildsWithoutFifteenFifteen)

toTuple :: Command -> (Int, Int, Int)
toTuple (Build x y z)     = (x, y, buildingToInt z)
toTuple (Deconstruct x y) = (x, y, -1)
toTuple NothingCommand    = (-1, -1, -1)  -- Works because no coords are negative on the board

instance Ord Command where
  (>=) x y = (toTuple x) >= (toTuple y)
  (<) x y = not (x >= y)
  (<=) x y = (toTuple x) <= (toTuple y)

buildingToInt :: BuildingType -> Int
buildingToInt DEFENSE = 0
buildingToInt ATTACK = 1
buildingToInt ENERGY = 2
buildingToInt TESLA = 4

oponentsDeconstructs :: [Command]
oponentsDeconstructs = [(Deconstruct 8  0),
                        (Deconstruct 8  1),
                        (Deconstruct 8  2),
                        (Deconstruct 8  3),
                        (Deconstruct 8  4),
                        (Deconstruct 8  5),
                        (Deconstruct 8  6),
                        (Deconstruct 8  7),
                        (Deconstruct 9  0),
                        (Deconstruct 9  1),
                        (Deconstruct 9  2),
                        (Deconstruct 9  3),
                        (Deconstruct 9  4),
                        (Deconstruct 9  5),
                        (Deconstruct 9  6),
                        (Deconstruct 9  7),
                        (Deconstruct 10 0),
                        (Deconstruct 10 1),
                        (Deconstruct 10 2),
                        (Deconstruct 10 3),
                        (Deconstruct 10 4),
                        (Deconstruct 10 5),
                        (Deconstruct 10 6),
                        (Deconstruct 10 7),
                        (Deconstruct 11 0),
                        (Deconstruct 11 1),
                        (Deconstruct 11 2),
                        (Deconstruct 11 3),
                        (Deconstruct 11 4),
                        (Deconstruct 11 5),
                        (Deconstruct 11 6),
                        (Deconstruct 11 7),
                        (Deconstruct 12 0),
                        (Deconstruct 12 1),
                        (Deconstruct 12 2),
                        (Deconstruct 12 3),
                        (Deconstruct 12 4),
                        (Deconstruct 12 5),
                        (Deconstruct 12 6),
                        (Deconstruct 12 7),
                        (Deconstruct 13 0),
                        (Deconstruct 13 1),
                        (Deconstruct 13 2),
                        (Deconstruct 13 3),
                        (Deconstruct 13 4),
                        (Deconstruct 13 5),
                        (Deconstruct 13 6),
                        (Deconstruct 13 7),
                        (Deconstruct 14 0),
                        (Deconstruct 14 1),
                        (Deconstruct 14 2),
                        (Deconstruct 14 3),
                        (Deconstruct 14 4),
                        (Deconstruct 14 5),
                        (Deconstruct 14 6),
                        (Deconstruct 14 7),
                        (Deconstruct 15 0),
                        (Deconstruct 15 1),
                        (Deconstruct 15 2),
                        (Deconstruct 15 3),
                        (Deconstruct 15 4),
                        (Deconstruct 15 5),
                        (Deconstruct 15 6),
                        (Deconstruct 15 7)]

myBuilds :: [Command]
myBuilds  = [(Build 0 0 DEFENSE),
             (Build 0 1 DEFENSE),
             (Build 0 2 DEFENSE),
             (Build 0 3 DEFENSE),
             (Build 0 4 DEFENSE),
             (Build 0 5 DEFENSE),
             (Build 0 6 DEFENSE),
             (Build 0 7 DEFENSE),
             (Build 1 0 DEFENSE),
             (Build 1 1 DEFENSE),
             (Build 1 2 DEFENSE),
             (Build 1 3 DEFENSE),
             (Build 1 4 DEFENSE),
             (Build 1 5 DEFENSE),
             (Build 1 6 DEFENSE),
             (Build 1 7 DEFENSE),
             (Build 2 0 DEFENSE),
             (Build 2 1 DEFENSE),
             (Build 2 2 DEFENSE),
             (Build 2 3 DEFENSE),
             (Build 2 4 DEFENSE),
             (Build 2 5 DEFENSE),
             (Build 2 6 DEFENSE),
             (Build 2 7 DEFENSE),
             (Build 3 0 DEFENSE),
             (Build 3 1 DEFENSE),
             (Build 3 2 DEFENSE),
             (Build 3 3 DEFENSE),
             (Build 3 4 DEFENSE),
             (Build 3 5 DEFENSE),
             (Build 3 6 DEFENSE),
             (Build 3 7 DEFENSE),
             (Build 4 0 DEFENSE),
             (Build 4 1 DEFENSE),
             (Build 4 2 DEFENSE),
             (Build 4 3 DEFENSE),
             (Build 4 4 DEFENSE),
             (Build 4 5 DEFENSE),
             (Build 4 6 DEFENSE),
             (Build 4 7 DEFENSE),
             (Build 5 0 DEFENSE),
             (Build 5 1 DEFENSE),
             (Build 5 2 DEFENSE),
             (Build 5 3 DEFENSE),
             (Build 5 4 DEFENSE),
             (Build 5 5 DEFENSE),
             (Build 5 6 DEFENSE),
             (Build 5 7 DEFENSE),
             (Build 6 0 DEFENSE),
             (Build 6 1 DEFENSE),
             (Build 6 2 DEFENSE),
             (Build 6 3 DEFENSE),
             (Build 6 4 DEFENSE),
             (Build 6 5 DEFENSE),
             (Build 6 6 DEFENSE),
             (Build 6 7 DEFENSE),
             (Build 7 0 DEFENSE),
             (Build 7 1 DEFENSE),
             (Build 7 2 DEFENSE),
             (Build 7 3 DEFENSE),
             (Build 7 4 DEFENSE),
             (Build 7 5 DEFENSE),
             (Build 7 6 DEFENSE),
             (Build 7 7 DEFENSE),
             (Build 0 0 ATTACK),
             (Build 0 1 ATTACK),
             (Build 0 2 ATTACK),
             (Build 0 3 ATTACK),
             (Build 0 4 ATTACK),
             (Build 0 5 ATTACK),
             (Build 0 6 ATTACK),
             (Build 0 7 ATTACK),
             (Build 1 0 ATTACK),
             (Build 1 1 ATTACK),
             (Build 1 2 ATTACK),
             (Build 1 3 ATTACK),
             (Build 1 4 ATTACK),
             (Build 1 5 ATTACK),
             (Build 1 6 ATTACK),
             (Build 1 7 ATTACK),
             (Build 2 0 ATTACK),
             (Build 2 1 ATTACK),
             (Build 2 2 ATTACK),
             (Build 2 3 ATTACK),
             (Build 2 4 ATTACK),
             (Build 2 5 ATTACK),
             (Build 2 6 ATTACK),
             (Build 2 7 ATTACK),
             (Build 3 0 ATTACK),
             (Build 3 1 ATTACK),
             (Build 3 2 ATTACK),
             (Build 3 3 ATTACK),
             (Build 3 4 ATTACK),
             (Build 3 5 ATTACK),
             (Build 3 6 ATTACK),
             (Build 3 7 ATTACK),
             (Build 4 0 ATTACK),
             (Build 4 1 ATTACK),
             (Build 4 2 ATTACK),
             (Build 4 3 ATTACK),
             (Build 4 4 ATTACK),
             (Build 4 5 ATTACK),
             (Build 4 6 ATTACK),
             (Build 4 7 ATTACK),
             (Build 5 0 ATTACK),
             (Build 5 1 ATTACK),
             (Build 5 2 ATTACK),
             (Build 5 3 ATTACK),
             (Build 5 4 ATTACK),
             (Build 5 5 ATTACK),
             (Build 5 6 ATTACK),
             (Build 5 7 ATTACK),
             (Build 6 0 ATTACK),
             (Build 6 1 ATTACK),
             (Build 6 2 ATTACK),
             (Build 6 3 ATTACK),
             (Build 6 4 ATTACK),
             (Build 6 5 ATTACK),
             (Build 6 6 ATTACK),
             (Build 6 7 ATTACK),
             (Build 7 0 ATTACK),
             (Build 7 1 ATTACK),
             (Build 7 2 ATTACK),
             (Build 7 3 ATTACK),
             (Build 7 4 ATTACK),
             (Build 7 5 ATTACK),
             (Build 7 6 ATTACK),
             (Build 7 7 ATTACK),
             (Build 0 0 ENERGY),
             (Build 0 1 ENERGY),
             (Build 0 2 ENERGY),
             (Build 0 3 ENERGY),
             (Build 0 4 ENERGY),
             (Build 0 5 ENERGY),
             (Build 0 6 ENERGY),
             (Build 0 7 ENERGY),
             (Build 1 0 ENERGY),
             (Build 1 1 ENERGY),
             (Build 1 2 ENERGY),
             (Build 1 3 ENERGY),
             (Build 1 4 ENERGY),
             (Build 1 5 ENERGY),
             (Build 1 6 ENERGY),
             (Build 1 7 ENERGY),
             (Build 2 0 ENERGY),
             (Build 2 1 ENERGY),
             (Build 2 2 ENERGY),
             (Build 2 3 ENERGY),
             (Build 2 4 ENERGY),
             (Build 2 5 ENERGY),
             (Build 2 6 ENERGY),
             (Build 2 7 ENERGY),
             (Build 3 0 ENERGY),
             (Build 3 1 ENERGY),
             (Build 3 2 ENERGY),
             (Build 3 3 ENERGY),
             (Build 3 4 ENERGY),
             (Build 3 5 ENERGY),
             (Build 3 6 ENERGY),
             (Build 3 7 ENERGY),
             (Build 4 0 ENERGY),
             (Build 4 1 ENERGY),
             (Build 4 2 ENERGY),
             (Build 4 3 ENERGY),
             (Build 4 4 ENERGY),
             (Build 4 5 ENERGY),
             (Build 4 6 ENERGY),
             (Build 4 7 ENERGY),
             (Build 5 0 ENERGY),
             (Build 5 1 ENERGY),
             (Build 5 2 ENERGY),
             (Build 5 3 ENERGY),
             (Build 5 4 ENERGY),
             (Build 5 5 ENERGY),
             (Build 5 6 ENERGY),
             (Build 5 7 ENERGY),
             (Build 6 0 ENERGY),
             (Build 6 1 ENERGY),
             (Build 6 2 ENERGY),
             (Build 6 3 ENERGY),
             (Build 6 4 ENERGY),
             (Build 6 5 ENERGY),
             (Build 6 6 ENERGY),
             (Build 6 7 ENERGY),
             (Build 7 0 ENERGY),
             (Build 7 1 ENERGY),
             (Build 7 2 ENERGY),
             (Build 7 3 ENERGY),
             (Build 7 4 ENERGY),
             (Build 7 5 ENERGY),
             (Build 7 6 ENERGY),
             (Build 7 7 ENERGY),
             (Build 0 0 TESLA),
             (Build 0 1 TESLA),
             (Build 0 2 TESLA),
             (Build 0 3 TESLA),
             (Build 0 4 TESLA),
             (Build 0 5 TESLA),
             (Build 0 6 TESLA),
             (Build 0 7 TESLA),
             (Build 1 0 TESLA),
             (Build 1 1 TESLA),
             (Build 1 2 TESLA),
             (Build 1 3 TESLA),
             (Build 1 4 TESLA),
             (Build 1 5 TESLA),
             (Build 1 6 TESLA),
             (Build 1 7 TESLA),
             (Build 2 0 TESLA),
             (Build 2 1 TESLA),
             (Build 2 2 TESLA),
             (Build 2 3 TESLA),
             (Build 2 4 TESLA),
             (Build 2 5 TESLA),
             (Build 2 6 TESLA),
             (Build 2 7 TESLA),
             (Build 3 0 TESLA),
             (Build 3 1 TESLA),
             (Build 3 2 TESLA),
             (Build 3 3 TESLA),
             (Build 3 4 TESLA),
             (Build 3 5 TESLA),
             (Build 3 6 TESLA),
             (Build 3 7 TESLA),
             (Build 4 0 TESLA),
             (Build 4 1 TESLA),
             (Build 4 2 TESLA),
             (Build 4 3 TESLA),
             (Build 4 4 TESLA),
             (Build 4 5 TESLA),
             (Build 4 6 TESLA),
             (Build 4 7 TESLA),
             (Build 5 0 TESLA),
             (Build 5 1 TESLA),
             (Build 5 2 TESLA),
             (Build 5 3 TESLA),
             (Build 5 4 TESLA),
             (Build 5 5 TESLA),
             (Build 5 6 TESLA),
             (Build 5 7 TESLA),
             (Build 6 0 TESLA),
             (Build 6 1 TESLA),
             (Build 6 2 TESLA),
             (Build 6 3 TESLA),
             (Build 6 4 TESLA),
             (Build 6 5 TESLA),
             (Build 6 6 TESLA),
             (Build 6 7 TESLA),
             (Build 7 0 TESLA),
             (Build 7 1 TESLA),
             (Build 7 2 TESLA),
             (Build 7 3 TESLA),
             (Build 7 4 TESLA),
             (Build 7 5 TESLA),
             (Build 7 6 TESLA),
             (Build 7 7 TESLA)]

oponentsBuilds :: [Command]
oponentsBuilds  = [(Build 8  0 DEFENSE),
                   (Build 8  1 DEFENSE),
                   (Build 8  2 DEFENSE),
                   (Build 8  3 DEFENSE),
                   (Build 8  4 DEFENSE),
                   (Build 8  5 DEFENSE),
                   (Build 8  6 DEFENSE),
                   (Build 8  7 DEFENSE),
                   (Build 9  0 DEFENSE),
                   (Build 9  1 DEFENSE),
                   (Build 9  2 DEFENSE),
                   (Build 9  3 DEFENSE),
                   (Build 9  4 DEFENSE),
                   (Build 9  5 DEFENSE),
                   (Build 9  6 DEFENSE),
                   (Build 9  7 DEFENSE),
                   (Build 10 0 DEFENSE),
                   (Build 10 1 DEFENSE),
                   (Build 10 2 DEFENSE),
                   (Build 10 3 DEFENSE),
                   (Build 10 4 DEFENSE),
                   (Build 10 5 DEFENSE),
                   (Build 10 6 DEFENSE),
                   (Build 10 7 DEFENSE),
                   (Build 11 0 DEFENSE),
                   (Build 11 1 DEFENSE),
                   (Build 11 2 DEFENSE),
                   (Build 11 3 DEFENSE),
                   (Build 11 4 DEFENSE),
                   (Build 11 5 DEFENSE),
                   (Build 11 6 DEFENSE),
                   (Build 11 7 DEFENSE),
                   (Build 12 0 DEFENSE),
                   (Build 12 1 DEFENSE),
                   (Build 12 2 DEFENSE),
                   (Build 12 3 DEFENSE),
                   (Build 12 4 DEFENSE),
                   (Build 12 5 DEFENSE),
                   (Build 12 6 DEFENSE),
                   (Build 12 7 DEFENSE),
                   (Build 13 0 DEFENSE),
                   (Build 13 1 DEFENSE),
                   (Build 13 2 DEFENSE),
                   (Build 13 3 DEFENSE),
                   (Build 13 4 DEFENSE),
                   (Build 13 5 DEFENSE),
                   (Build 13 6 DEFENSE),
                   (Build 13 7 DEFENSE),
                   (Build 14 0 DEFENSE),
                   (Build 14 1 DEFENSE),
                   (Build 14 2 DEFENSE),
                   (Build 14 3 DEFENSE),
                   (Build 14 4 DEFENSE),
                   (Build 14 5 DEFENSE),
                   (Build 14 6 DEFENSE),
                   (Build 14 7 DEFENSE),
                   (Build 15 0 DEFENSE),
                   (Build 15 1 DEFENSE),
                   (Build 15 2 DEFENSE),
                   (Build 15 3 DEFENSE),
                   (Build 15 4 DEFENSE),
                   (Build 15 5 DEFENSE),
                   (Build 15 6 DEFENSE),
                   (Build 15 7 DEFENSE),
                   (Build 8  0 ATTACK),
                   (Build 8  1 ATTACK),
                   (Build 8  2 ATTACK),
                   (Build 8  3 ATTACK),
                   (Build 8  4 ATTACK),
                   (Build 8  5 ATTACK),
                   (Build 8  6 ATTACK),
                   (Build 8  7 ATTACK),
                   (Build 9  0 ATTACK),
                   (Build 9  1 ATTACK),
                   (Build 9  2 ATTACK),
                   (Build 9  3 ATTACK),
                   (Build 9  4 ATTACK),
                   (Build 9  5 ATTACK),
                   (Build 9  6 ATTACK),
                   (Build 9  7 ATTACK),
                   (Build 10 0 ATTACK),
                   (Build 10 1 ATTACK),
                   (Build 10 2 ATTACK),
                   (Build 10 3 ATTACK),
                   (Build 10 4 ATTACK),
                   (Build 10 5 ATTACK),
                   (Build 10 6 ATTACK),
                   (Build 10 7 ATTACK),
                   (Build 11 0 ATTACK),
                   (Build 11 1 ATTACK),
                   (Build 11 2 ATTACK),
                   (Build 11 3 ATTACK),
                   (Build 11 4 ATTACK),
                   (Build 11 5 ATTACK),
                   (Build 11 6 ATTACK),
                   (Build 11 7 ATTACK),
                   (Build 12 0 ATTACK),
                   (Build 12 1 ATTACK),
                   (Build 12 2 ATTACK),
                   (Build 12 3 ATTACK),
                   (Build 12 4 ATTACK),
                   (Build 12 5 ATTACK),
                   (Build 12 6 ATTACK),
                   (Build 12 7 ATTACK),
                   (Build 13 0 ATTACK),
                   (Build 13 1 ATTACK),
                   (Build 13 2 ATTACK),
                   (Build 13 3 ATTACK),
                   (Build 13 4 ATTACK),
                   (Build 13 5 ATTACK),
                   (Build 13 6 ATTACK),
                   (Build 13 7 ATTACK),
                   (Build 14 0 ATTACK),
                   (Build 14 1 ATTACK),
                   (Build 14 2 ATTACK),
                   (Build 14 3 ATTACK),
                   (Build 14 4 ATTACK),
                   (Build 14 5 ATTACK),
                   (Build 14 6 ATTACK),
                   (Build 14 7 ATTACK),
                   (Build 15 0 ATTACK),
                   (Build 15 1 ATTACK),
                   (Build 15 2 ATTACK),
                   (Build 15 3 ATTACK),
                   (Build 15 4 ATTACK),
                   (Build 15 5 ATTACK),
                   (Build 15 6 ATTACK),
                   (Build 15 7 ATTACK),
                   (Build 8  0 ENERGY),
                   (Build 8  1 ENERGY),
                   (Build 8  2 ENERGY),
                   (Build 8  3 ENERGY),
                   (Build 8  4 ENERGY),
                   (Build 8  5 ENERGY),
                   (Build 8  6 ENERGY),
                   (Build 8  7 ENERGY),
                   (Build 9  0 ENERGY),
                   (Build 9  1 ENERGY),
                   (Build 9  2 ENERGY),
                   (Build 9  3 ENERGY),
                   (Build 9  4 ENERGY),
                   (Build 9  5 ENERGY),
                   (Build 9  6 ENERGY),
                   (Build 9  7 ENERGY),
                   (Build 10 0 ENERGY),
                   (Build 10 1 ENERGY),
                   (Build 10 2 ENERGY),
                   (Build 10 3 ENERGY),
                   (Build 10 4 ENERGY),
                   (Build 10 5 ENERGY),
                   (Build 10 6 ENERGY),
                   (Build 10 7 ENERGY),
                   (Build 11 0 ENERGY),
                   (Build 11 1 ENERGY),
                   (Build 11 2 ENERGY),
                   (Build 11 3 ENERGY),
                   (Build 11 4 ENERGY),
                   (Build 11 5 ENERGY),
                   (Build 11 6 ENERGY),
                   (Build 11 7 ENERGY),
                   (Build 12 0 ENERGY),
                   (Build 12 1 ENERGY),
                   (Build 12 2 ENERGY),
                   (Build 12 3 ENERGY),
                   (Build 12 4 ENERGY),
                   (Build 12 5 ENERGY),
                   (Build 12 6 ENERGY),
                   (Build 12 7 ENERGY),
                   (Build 13 0 ENERGY),
                   (Build 13 1 ENERGY),
                   (Build 13 2 ENERGY),
                   (Build 13 3 ENERGY),
                   (Build 13 4 ENERGY),
                   (Build 13 5 ENERGY),
                   (Build 13 6 ENERGY),
                   (Build 13 7 ENERGY),
                   (Build 14 0 ENERGY),
                   (Build 14 1 ENERGY),
                   (Build 14 2 ENERGY),
                   (Build 14 3 ENERGY),
                   (Build 14 4 ENERGY),
                   (Build 14 5 ENERGY),
                   (Build 14 6 ENERGY),
                   (Build 14 7 ENERGY),
                   (Build 15 0 ENERGY),
                   (Build 15 1 ENERGY),
                   (Build 15 2 ENERGY),
                   (Build 15 3 ENERGY),
                   (Build 15 4 ENERGY),
                   (Build 15 5 ENERGY),
                   (Build 15 6 ENERGY),
                   (Build 15 7 ENERGY),
                   (Build 8  0 TESLA),
                   (Build 8  1 TESLA),
                   (Build 8  2 TESLA),
                   (Build 8  3 TESLA),
                   (Build 8  4 TESLA),
                   (Build 8  5 TESLA),
                   (Build 8  6 TESLA),
                   (Build 8  7 TESLA),
                   (Build 9  0 TESLA),
                   (Build 9  1 TESLA),
                   (Build 9  2 TESLA),
                   (Build 9  3 TESLA),
                   (Build 9  4 TESLA),
                   (Build 9  5 TESLA),
                   (Build 9  6 TESLA),
                   (Build 9  7 TESLA),
                   (Build 10 0 TESLA),
                   (Build 10 1 TESLA),
                   (Build 10 2 TESLA),
                   (Build 10 3 TESLA),
                   (Build 10 4 TESLA),
                   (Build 10 5 TESLA),
                   (Build 10 6 TESLA),
                   (Build 10 7 TESLA),
                   (Build 11 0 TESLA),
                   (Build 11 1 TESLA),
                   (Build 11 2 TESLA),
                   (Build 11 3 TESLA),
                   (Build 11 4 TESLA),
                   (Build 11 5 TESLA),
                   (Build 11 6 TESLA),
                   (Build 11 7 TESLA),
                   (Build 12 0 TESLA),
                   (Build 12 1 TESLA),
                   (Build 12 2 TESLA),
                   (Build 12 3 TESLA),
                   (Build 12 4 TESLA),
                   (Build 12 5 TESLA),
                   (Build 12 6 TESLA),
                   (Build 12 7 TESLA),
                   (Build 13 0 TESLA),
                   (Build 13 1 TESLA),
                   (Build 13 2 TESLA),
                   (Build 13 3 TESLA),
                   (Build 13 4 TESLA),
                   (Build 13 5 TESLA),
                   (Build 13 6 TESLA),
                   (Build 13 7 TESLA),
                   (Build 14 0 TESLA),
                   (Build 14 1 TESLA),
                   (Build 14 2 TESLA),
                   (Build 14 3 TESLA),
                   (Build 14 4 TESLA),
                   (Build 14 5 TESLA),
                   (Build 14 6 TESLA),
                   (Build 14 7 TESLA),
                   (Build 15 0 TESLA),
                   (Build 15 1 TESLA),
                   (Build 15 2 TESLA),
                   (Build 15 3 TESLA),
                   (Build 15 4 TESLA),
                   (Build 15 5 TESLA),
                   (Build 15 6 TESLA),
                   (Build 15 7 TESLA)]

myDeconstructs :: [Command]
myDeconstructs  = [(Deconstruct 0 0),
                   (Deconstruct 0 1),
                   (Deconstruct 0 2),
                   (Deconstruct 0 3),
                   (Deconstruct 0 4),
                   (Deconstruct 0 5),
                   (Deconstruct 0 6),
                   (Deconstruct 0 7),
                   (Deconstruct 1 0),
                   (Deconstruct 1 1),
                   (Deconstruct 1 2),
                   (Deconstruct 1 3),
                   (Deconstruct 1 4),
                   (Deconstruct 1 5),
                   (Deconstruct 1 6),
                   (Deconstruct 1 7),
                   (Deconstruct 2 0),
                   (Deconstruct 2 1),
                   (Deconstruct 2 2),
                   (Deconstruct 2 3),
                   (Deconstruct 2 4),
                   (Deconstruct 2 5),
                   (Deconstruct 2 6),
                   (Deconstruct 2 7),
                   (Deconstruct 3 0),
                   (Deconstruct 3 1),
                   (Deconstruct 3 2),
                   (Deconstruct 3 3),
                   (Deconstruct 3 4),
                   (Deconstruct 3 5),
                   (Deconstruct 3 6),
                   (Deconstruct 3 7),
                   (Deconstruct 4 0),
                   (Deconstruct 4 1),
                   (Deconstruct 4 2),
                   (Deconstruct 4 3),
                   (Deconstruct 4 4),
                   (Deconstruct 4 5),
                   (Deconstruct 4 6),
                   (Deconstruct 4 7),
                   (Deconstruct 5 0),
                   (Deconstruct 5 1),
                   (Deconstruct 5 2),
                   (Deconstruct 5 3),
                   (Deconstruct 5 4),
                   (Deconstruct 5 5),
                   (Deconstruct 5 6),
                   (Deconstruct 5 7),
                   (Deconstruct 6 0),
                   (Deconstruct 6 1),
                   (Deconstruct 6 2),
                   (Deconstruct 6 3),
                   (Deconstruct 6 4),
                   (Deconstruct 6 5),
                   (Deconstruct 6 6),
                   (Deconstruct 6 7),
                   (Deconstruct 7 0),
                   (Deconstruct 7 1),
                   (Deconstruct 7 2),
                   (Deconstruct 7 3),
                   (Deconstruct 7 4),
                   (Deconstruct 7 5),
                   (Deconstruct 7 6),
                   (Deconstruct 7 7)]

myFullRow :: Row
myFullRow = M.fromList [(0, (Building { integrity              = 20,
                                        weaponCooldownTimeLeft = 0,
                                        buildingType           = DEFENSE })),
                        (1, (Building { integrity              = 20,
                                        weaponCooldownTimeLeft = 0,
                                        buildingType           = DEFENSE })),
                        (2, (Building { integrity              = 20,
                                        weaponCooldownTimeLeft = 0,
                                        buildingType           = DEFENSE })),
                        (3, (Building { integrity              = 10,
                                        weaponCooldownTimeLeft = 0,
                                        buildingType           = DEFENSE })),
                        (4, (Building { integrity              = 20,
                                        weaponCooldownTimeLeft = 0,
                                        buildingType           = DEFENSE })),
                        (5, (Building { integrity              = 20,
                                        weaponCooldownTimeLeft = 0,
                                        buildingType           = DEFENSE })),
                        (6, (Building { integrity              = 10,
                                        weaponCooldownTimeLeft = 0,
                                        buildingType           = DEFENSE })),
                        (7, (Building { integrity              = 10,
                                        weaponCooldownTimeLeft = 0,
                                        buildingType           = DEFENSE }))]

oponentsFullRow :: Row
oponentsFullRow = M.fromList [(8, (Building { integrity              = 20,
                                              weaponCooldownTimeLeft = 0,
                                              buildingType           = DEFENSE })),
                              (9, (Building { integrity              = 20,
                                              weaponCooldownTimeLeft = 0,
                                              buildingType           = DEFENSE })),
                              (10, (Building { integrity              = 20,
                                               weaponCooldownTimeLeft = 0,
                                               buildingType           = DEFENSE })),
                              (11, (Building { integrity              = 10,
                                               weaponCooldownTimeLeft = 0,
                                               buildingType           = DEFENSE })),
                              (12, (Building { integrity              = 20,
                                               weaponCooldownTimeLeft = 0,
                                               buildingType           = DEFENSE })),
                              (13, (Building { integrity              = 20,
                                               weaponCooldownTimeLeft = 0,
                                               buildingType           = DEFENSE })),
                              (14, (Building { integrity              = 10,
                                               weaponCooldownTimeLeft = 0,
                                               buildingType           = DEFENSE })),
                              (15, (Building { integrity              = 10,
                                               weaponCooldownTimeLeft = 0,
                                               buildingType           = DEFENSE }))]
  
myFullMap :: TowerMap
myFullMap = M.fromList [(0, myFullRow),
                        (1, myFullRow),
                        (2, myFullRow),
                        (3, myFullRow),
                        (4, myFullRow),
                        (5, myFullRow),
                        (6, myFullRow),
                        (7, myFullRow)]
             
oponentsFullMap :: TowerMap
oponentsFullMap = M.fromList [(0, oponentsFullRow),
                              (1, oponentsFullRow),
                              (2, oponentsFullRow),
                              (3, oponentsFullRow),
                              (4, oponentsFullRow),
                              (5, oponentsFullRow),
                              (6, oponentsFullRow),
                              (7, oponentsFullRow)]

fullBoard :: GameState
fullBoard =
  GameState { me      = (Player { energy            = 37,
                                  health            = 30,
                                  hitsTaken         = 14,
                                  score             = 451,
                                  towerMap          = myFullMap,
                                  constructionQueue = PQ.empty,
                                  ownedMissiles     =  []}),
              oponent = (Player { energy            = 53,
                                  health            = 100,
                                  hitsTaken         = 0,
                                  score             = 2093,
                                  towerMap          = oponentsFullMap,
                                  constructionQueue = PQ.empty,
                                  ownedMissiles     = [] })}

aDefenseBuilding :: Building
aDefenseBuilding = (Building { integrity              = 10,
                               weaponCooldownTimeLeft = 0,
                               buildingType           = DEFENSE })

emptyMe :: Player
emptyMe = (Player { energy            = 400,
                    health            = 30,
                    hitsTaken         = 14,
                    score             = 451,
                    towerMap          = M.empty,
                    constructionQueue = PQ.empty,
                    ownedMissiles     =  []})

emptyOponent :: Player
emptyOponent = (Player { energy            = 400,
                         health            = 100,
                         hitsTaken         = 0,
                         score             = 2093,
                         towerMap          = M.empty,
                         constructionQueue = PQ.empty,
                         ownedMissiles     = [] })

emptyBoard :: GameState
emptyBoard =
  GameState { me      = emptyMe,
              oponent = emptyOponent }

emptyBoardWithBuildingAtSevenSeven :: GameState
emptyBoardWithBuildingAtSevenSeven = emptyBoard { me      = ( emptyMe { towerMap = M.fromList [
                                                                          (7, (M.fromList [(7, aDefenseBuilding)]))] } ),
                                                  oponent = ( emptyMe { towerMap = M.fromList [
                                                                          (7, (M.fromList [(15, aDefenseBuilding)]))] } ) }

isAt :: Int -> Int -> Command -> Bool
isAt x y (Build x' y' _)     = x == x' && y == y'
isAt x y (Deconstruct x' y') = x == x' && y == y'
isAt _ _ NothingCommand      = False

isntAt :: Int -> Int -> Command -> Bool
isntAt x y = not . isAt x y

myBuildsWithoutSevenSeven :: [Command]
myBuildsWithoutSevenSeven = (Deconstruct 7 7) : filter (isntAt 7 7) myBuilds

oponentsBuildsWithoutFifteenFifteen :: [Command]
oponentsBuildsWithoutFifteenFifteen = (Deconstruct 15 7) : filter (isntAt 15 7) oponentsBuilds