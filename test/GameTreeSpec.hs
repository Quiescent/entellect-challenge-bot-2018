module GameTreeSpec
  where

import Data.Maybe
import qualified Data.Vector.Unboxed as UV
import qualified Data.IntMap         as M

import GameTree

import Test.Hspec

spec :: Spec
spec =
  subTreeSpec              >>
  incrementDecrementBySpec >>
  addAtSpec

simpleSubTree :: GameTree
simpleSubTree = (GameTree (UV.fromList [1..2]) M.empty (UV.fromList [1..3]))

subTreeSpec :: Spec
subTreeSpec = do
  describe "subTree" $ do
    it "should produce nothing for the empty tree" $
      subTree [0] empty == Nothing
    it "should have a tree at 1 if one is inserted at 1" $
      (fromJust $ subTree [1] (addAt [1] simpleSubTree empty)) == simpleSubTree

incrementDecrementBySpec :: Spec
incrementDecrementBySpec = do
  describe "incrementDecrementBy" $ do
    it "should be implemented!" $
      True == False

addAtSpec :: Spec
addAtSpec = do
  describe "addAt" $ do
    it "should be implemented!" $
      True == False

