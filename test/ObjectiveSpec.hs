module ObjectiveSpec where

import Interpretor
import Objective
import BitSetMap
import Coord

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
    it "should produce a score of 1 when there is an energy tower to destroy on the other side" $
      myIntermediateBoardScore (emptyState {
                                   me = emptyPlayer {
                                       allTowers      = addBuilding (toCoord 0 2) 0,
                                       allBuiltTowers = addBuilding (toCoord 0 2) 0,
                                       attack0Towers  = addBuilding (toCoord 0 2) 0 },
                                   oponent = emptyPlayer {
                                       allTowers      = addBuilding (toCoord 0 2) 0,
                                       allBuiltTowers = addBuilding (toCoord 0 2) 0,
                                       energyTowers   = addBuilding (toCoord 0 2) 0 }})
      `shouldBe`
      1
    it "should produce a score of 2 when there are two energy towers in a row (if the tower is far enough forward)" $
      myIntermediateBoardScore (emptyState {
                                   me = emptyPlayer {
                                       allTowers      = addBuilding (toCoord 3 2) 0,
                                       allBuiltTowers = addBuilding (toCoord 3 2) 0,
                                       attack0Towers  = addBuilding (toCoord 3 2) 0 },
                                   oponent = emptyPlayer {
                                       allTowers      = addBuilding (toCoord 0 2)
                                                        (addBuilding (toCoord 3 2) 0),
                                       allBuiltTowers = addBuilding (toCoord 0 2)
                                                        (addBuilding (toCoord 3 2) 0),
                                       energyTowers   = addBuilding (toCoord 0 2)
                                                        (addBuilding (toCoord 3 2) 0) }})
      `shouldBe`
      2

oponentsIntermediateBoardScoreSpec :: Spec
oponentsIntermediateBoardScoreSpec = do
  describe "oponentsIntermediateBoardScore" $ do
    it "should produce zero given an empty board" $
      oponentsIntermediateBoardScore emptyState
      `shouldBe`
      0
    it "should produce a score of 1 when there is an energy tower to destroy on the other side" $
      oponentsIntermediateBoardScore (emptyState {
                                         oponent = emptyPlayer {
                                             allTowers      = addBuilding (toCoord 0 2) 0,
                                             allBuiltTowers = addBuilding (toCoord 0 2) 0,
                                             attack0Towers  = addBuilding (toCoord 0 2) 0 },
                                         me = emptyPlayer {
                                             allTowers      = addBuilding (toCoord 0 2) 0,
                                             allBuiltTowers = addBuilding (toCoord 0 2) 0,
                                             energyTowers   = addBuilding (toCoord 0 2) 0 }})
      `shouldBe`
      1
    it "should produce a score of 2 when there are two energy towers in a row (if the tower is far enough forward)" $
      oponentsIntermediateBoardScore (emptyState {
                                         oponent = emptyPlayer {
                                             allTowers      = addBuilding (toCoord 3 2) 0,
                                             allBuiltTowers = addBuilding (toCoord 3 2) 0,
                                             attack0Towers  = addBuilding (toCoord 3 2) 0 },
                                         me = emptyPlayer {
                                             allTowers      = addBuilding (toCoord 0 2)
                                                              (addBuilding (toCoord 3 2) 0),
                                             allBuiltTowers = addBuilding (toCoord 0 2)
                                                              (addBuilding (toCoord 3 2) 0),
                                             energyTowers   = addBuilding (toCoord 0 2)
                                                              (addBuilding (toCoord 3 2) 0) }})
      `shouldBe`
      2
