module GameTreeSpec
  where

import Data.Maybe
import qualified Data.Vector.Unboxed as UV
import qualified Data.IntMap         as M
import qualified Data.List           as L

import GameTree
import EfficientCommand

import Test.Hspec
import Test.QuickCheck

import Debug.Trace

spec :: Spec
spec =
  subTreeSpec              >>
  incrementDecrementBySpec >>
  addAtSpec

simpleSubTree :: GameTree
simpleSubTree = GameTree (UV.fromList [1..2]) M.empty (UV.fromList [1..3])

incrementDecrementedSimpleSubTree :: GameTree
incrementDecrementedSimpleSubTree = GameTree (UV.fromList [2, 2]) M.empty (UV.fromList [0, 2, 3])

secondIncrementDecrementedSimpleSubTree :: GameTree
secondIncrementDecrementedSimpleSubTree = GameTree (UV.fromList [1, 3]) M.empty (UV.fromList [1, 1, 3])

zeroZeroTree :: GameTree
zeroZeroTree =
  GameTree
  (UV.fromList [1..2])
  (M.fromList [(0, (GameTree
                     (UV.fromList [1..2])
                     (M.fromList [(0, simpleSubTree)])
                     (UV.fromList [1..2])))])
  (UV.fromList [1..3])

zeroOneTree :: GameTree
zeroOneTree =
  GameTree
  (UV.fromList [1..2])
  (M.fromList [(0, (GameTree
                     (UV.fromList [1..2])
                     (M.fromList [(1, simpleSubTree)])
                     (UV.fromList [1..2])))])
  (UV.fromList [1..3])

probe message x = trace (message ++ show x) x

addEmptyPath :: [PackedCommand] -> GameTree -> GameTree
addEmptyPath path tree =
  probe ("After adding: " ++ show path) $
  foldr (\ newPath tree' -> addAt newPath empty tree') tree $
  tail $
  reverse $
  L.tails $
  reverse $
  tail $
  reverse path

-- Not working :(
elementsAddedAreFound :: Spec
elementsAddedAreFound = do
  describe "gameTrees" $ do
    it "should add elements and have them found at the same location later" $
      -- You have to add the path all the way because I wanted the
      -- logic to be able to ignore adding them since the tree will
      -- always expand from the root one more than it's been so far.
      property $ \ xs ->
                   (subTree xs $ addAt xs simpleSubTree (addEmptyPath xs empty))
                   ==
                   (Just simpleSubTree)

subTreeSpec :: Spec
subTreeSpec = do
  describe "subTree" $ do
    it "should produce nothing for the empty tree" $
      subTree [0] empty == Nothing
    it "should have a tree at 1 if one is inserted at 1" $
      (fromJust $ subTree [1] (addAt [1] simpleSubTree empty)) == simpleSubTree
    it "should find an alement at [0,0]" $
      (fromJust $ subTree [0, 0] zeroZeroTree) == simpleSubTree
    it "should find an alement at [0,1]" $
      (fromJust $ subTree [0, 1] zeroOneTree) == simpleSubTree

incrementDecrementBySpec :: Spec
incrementDecrementBySpec = do
  describe "incrementDecrementBy" $ do
    it "should do nothing with an empty list of moves" $
      incrementDecrementBy [] 1 simpleSubTree == simpleSubTree
    it "should increment the fitness of the first move given 0 and decrement the corresponding oponent move" $
      incrementDecrementBy [0] 1 simpleSubTree == incrementDecrementedSimpleSubTree
    it "should increment the fitness of the second move given 1 and decrement the corresponding oponent move" $
      incrementDecrementBy [513] 1 simpleSubTree == secondIncrementDecrementedSimpleSubTree
    it "should increment the fitness of moves two deep" $
      incrementDecrementBy [512, 1] 1 unincrementedZeroOneTree == incrementedZeroOneTree

unincrementedZeroOneTree :: GameTree
unincrementedZeroOneTree =
  GameTree
  (UV.fromList [0, 1, 2, 3, 4])
  (M.fromList [(512, (GameTree
                       (UV.fromList [1, 2, 3, 4, 5, 6])
                       M.empty
                       (UV.fromList [2, 3, 4, 5, 6, 7])))])
  (UV.fromList [1, 2, 3, 4, 5])

incrementedZeroOneTree :: GameTree
incrementedZeroOneTree =
  GameTree
  (UV.fromList [1, 1, 2, 3, 4])
  (M.fromList [(512, (GameTree
                       (UV.fromList [1, 3, 3, 4, 5, 6])
                       M.empty
                       (UV.fromList [1, 3, 4, 5, 6, 7])))])
  (UV.fromList [1, 1, 3, 4, 5])


emptyZeroOneTree :: GameTree
emptyZeroOneTree = GameTree
                   UV.empty
                   (M.fromList [(0, (GameTree
                                      UV.empty
                                      M.empty
                                      UV.empty))])
                   UV.empty

emptyZeroOneTreeWithSimpleAdded :: GameTree
emptyZeroOneTreeWithSimpleAdded =
  GameTree
  UV.empty
  (M.fromList [(0, (GameTree
                     UV.empty
                     (M.fromList [(1, simpleSubTree)])
                     UV.empty))])
  UV.empty

emptyZeroZeroZeroTree :: GameTree
emptyZeroZeroZeroTree =
  GameTree
  UV.empty
  (M.fromList [(0, (GameTree
                     UV.empty
                     (M.fromList [(0, (GameTree
                                        UV.empty
                                        M.empty
                                        UV.empty))])
                     UV.empty))])
  UV.empty

emptyZeroZeroZeroTreeWithSimpleAdded :: GameTree
emptyZeroZeroZeroTreeWithSimpleAdded =
  GameTree
  UV.empty
  (M.fromList [(0, (GameTree
                     UV.empty
                     (M.fromList [(0, (GameTree
                                        UV.empty
                                        (M.fromList [(0, simpleSubTree)])
                                        UV.empty))])
                     UV.empty))])
  UV.empty

addAtSpec :: Spec
addAtSpec = do
  describe "addAt" $ do
    it "should add a result in the subtree when adding an empty path" $
      addAt [] simpleSubTree empty == simpleSubTree
    it "should add at a nested node" $
      addAt [0, 1] simpleSubTree emptyZeroOneTree == emptyZeroOneTreeWithSimpleAdded
    it "should add at a three-nested node" $
      (addAt [0, 0, 0] simpleSubTree emptyZeroZeroZeroTree) == emptyZeroZeroZeroTreeWithSimpleAdded
