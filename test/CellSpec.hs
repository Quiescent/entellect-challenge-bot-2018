module CellSpec where

import Test.Hspec

import Cell
import Coord

spec :: Spec
spec =
  cellBelongsToMeSpec      >>
  cellBelongsToOponentSpec

cellBelongsToMeSpec :: Spec
cellBelongsToMeSpec = do
  describe "cellBelongsToMe" $ do
    it "should produce true for a cell in the middle" $
      cellBelongsToMe (toCoord 7 3) `shouldBe` True
    it "should produce true for a cell on the extreme left" $
      cellBelongsToMe (toCoord 0 3) `shouldBe` True
    it "should produce false for a cell in the middle on the right" $
      cellBelongsToMe (toCoord 8 5) `shouldBe` False
    it "should produce false for a cell on the extreme right" $
      cellBelongsToMe (toCoord 15 2) `shouldBe` False

cellBelongsToOponentSpec :: Spec
cellBelongsToOponentSpec = do
  describe "cellBelongsToOponent" $ do
    it "should produce true for a cell in the middle" $
      cellBelongsToOponent (toCoord 7 3) `shouldBe` False
    it "should produce true for a cell on the extreoponent left" $
      cellBelongsToOponent (toCoord 0 3) `shouldBe` False
    it "should produce false for a cell in the middle on the right" $
      cellBelongsToOponent (toCoord 8 5) `shouldBe` True
    it "should produce false for a cell on the extreoponent right" $
      cellBelongsToOponent (toCoord 15 2) `shouldBe` True

