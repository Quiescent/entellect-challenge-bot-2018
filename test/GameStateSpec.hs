module GameStateSpec where

import Test.Hspec

import GameState

spec :: Spec
spec =
  runCommandSpec                 >>
  mapMyPlayerSpec                >>
  mapOponentsPlayerSpec          >>
  updateMyMissilesSpec           >>
  updateOponentsMissilesSpec     >>
  mapMyMapSpec                   >>
  mapOponentsMapSpec             >>
  updateMeSpec                   >>
  updateOponentSpec              >>
  incrementMyHitsTakenSpec       >>
  incrementOponentsHitsTakenSpec >>
  buildForMeSpec                 >>
  buildForOponentSpec            >>
  updateMyMoveSpec               >>
  updateOponentsMoveSpec

runCommandSpec :: Spec
runCommandSpec = do
  describe "runCommand" $ do
    it "should be implemented" $ 0 `shouldBe` 1

mapMyPlayerSpec :: Spec
mapMyPlayerSpec = do
  describe "mapMyPlayer" $ do
    it "should be implemented" $ 0 `shouldBe` 1

mapOponentsPlayerSpec :: Spec
mapOponentsPlayerSpec = do
  describe "mapOponentsPlayer" $ do
    it "should be implemented" $ 0 `shouldBe` 1

updateMyMissilesSpec :: Spec
updateMyMissilesSpec = do
  describe "updateMyMissiles" $ do
    it "should be implemented" $ 0 `shouldBe` 1

updateOponentsMissilesSpec :: Spec
updateOponentsMissilesSpec = do
  describe "updateOponentsMissiles" $ do
    it "should be implemented" $ 0 `shouldBe` 1

mapMyMapSpec :: Spec
mapMyMapSpec = do
  describe "mapMyMap" $ do
    it "should be implemented" $ 0 `shouldBe` 1

mapOponentsMapSpec :: Spec
mapOponentsMapSpec = do
  describe "mapOponentsMap" $ do
    it "should be implemented" $ 0 `shouldBe` 1

updateMeSpec :: Spec
updateMeSpec = do
  describe "updateMe" $ do
    it "should be implemented" $ 0 `shouldBe` 1

updateOponentSpec :: Spec
updateOponentSpec = do
  describe "updateOponent" $ do
    it "should be implemented" $ 0 `shouldBe` 1

incrementMyHitsTakenSpec :: Spec
incrementMyHitsTakenSpec = do
  describe "incrementMyHitsTaken" $ do
    it "should be implemented" $ 0 `shouldBe` 1

incrementOponentsHitsTakenSpec :: Spec
incrementOponentsHitsTakenSpec = do
  describe "incrementOponentsHitsTaken" $ do
    it "should be implemented" $ 0 `shouldBe` 1

buildForMeSpec :: Spec
buildForMeSpec = do
  describe "buildForMe" $ do
    it "should be implemented" $ 0 `shouldBe` 1

buildForOponentSpec :: Spec
buildForOponentSpec = do
  describe "buildForOponent" $ do
    it "should be implemented" $ 0 `shouldBe` 1

updateMyMoveSpec :: Spec
updateMyMoveSpec = do
  describe "updateMyMove" $ do
    it "should be implemented" $ 0 `shouldBe` 1

updateOponentsMoveSpec :: Spec
updateOponentsMoveSpec = do
  describe "updateOponentsMove" $ do
    it "should be implemented" $ 0 `shouldBe` 1
