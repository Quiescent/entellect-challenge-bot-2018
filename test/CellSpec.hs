module CellSpec where

import Test.Hspec

import Cell

spec :: Spec
spec =
  allCellsSpec             >>
  cellBelongsToMeSpec      >>
  cellBelongsToOponentSpec

allCellsSpec :: Spec
allCellsSpec = do
  describe "allCells" $ do
    it "should be implemented!" $ 0 `shouldBe` 1

cellBelongsToMeSpec :: Spec
cellBelongsToMeSpec = do
  describe "cellBelongsToMe" $ do
    it "should be implemented!" $ 0 `shouldBe` 1

cellBelongsToOponentSpec :: Spec
cellBelongsToOponentSpec = do
  describe "cellBelongsToOponent" $ do
    it "should be implemented!" $ 0 `shouldBe` 1

