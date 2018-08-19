module ObjectiveSpec where

import Interpretor
import Objective

import Test.Hspec

emptyState :: GameState
emptyState = GameState 0 emptyPlayer emptyPlayer

spec :: Spec
spec =
  myIntermediateBoardScoreSpec >>
  oponentsIntermediateBoardScoreSpec

myIntermediateBoardScoreSpec :: Spec
myIntermediateBoardScoreSpec = do
  describe "myIntermediateBoardScore" $ do
    it "should produce zero given an empty board" $
      myIntermediateBoardScore emptyState
      `shouldBe`
      0

oponentsIntermediateBoardScoreSpec :: Spec
oponentsIntermediateBoardScoreSpec = do
  describe "oponentsIntermediateBoardScore" $ do
    it "should produce zero given an empty board" $
      oponentsIntermediateBoardScore emptyState
      `shouldBe`
      0
