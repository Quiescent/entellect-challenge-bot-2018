module CellSpec where

import Test.Hspec

import Cell

spec :: Spec
spec =
  cellBelongsToMeSpec      >>
  cellBelongsToOponentSpec

cellBelongsToMeSpec :: Spec
cellBelongsToMeSpec = do
  describe "cellBelongsToMe" $ do
    it "should produce true for a cell in the middle" $
      cellBelongsToMe (7, 3) `shouldBe` True
    it "should produce true for a cell on the extreme left" $
      cellBelongsToMe (0, 3) `shouldBe` True
    it "should produce false for a cell in the middle on the right" $
      cellBelongsToMe (8, 5) `shouldBe` False
    it "should produce false for a cell on the extreme right" $
      cellBelongsToMe (15, 2) `shouldBe` False

cellBelongsToOponentSpec :: Spec
cellBelongsToOponentSpec = do
  describe "cellBelongsToOponent" $ do
    it "should produce true for a cell in the middle" $
      cellBelongsToOponent (7, 3) `shouldBe` False
    it "should produce true for a cell on the extreoponent left" $
      cellBelongsToOponent (0, 3) `shouldBe` False
    it "should produce false for a cell in the middle on the right" $
      cellBelongsToOponent (8, 5) `shouldBe` True
    it "should produce false for a cell on the extreoponent right" $
      cellBelongsToOponent (15, 2) `shouldBe` True

