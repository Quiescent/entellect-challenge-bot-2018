module RowSpec where

import Row

import Test.Hspec

spec :: Spec
spec =
  rowAtSpec           >>
  rowFoldrSpec        >>
  rowFoldlSpec        >>
  rowFoldrIndexedSpec

rowAtSpec :: Spec
rowAtSpec = do
  describe "rowAtSpec" $ do
    it "should be implemented" $ 0 `shouldBe` 1

rowFoldrSpec :: Spec
rowFoldrSpec = do
  describe "rowFoldrSpec" $ do
    it "should be implemented" $ 0 `shouldBe` 1

rowFoldlSpec :: Spec
rowFoldlSpec = do
  describe "rowFoldlSpec" $ do
    it "should be implemented" $ 0 `shouldBe` 1

rowFoldrIndexedSpec :: Spec
rowFoldrIndexedSpec = do
  describe "rowFoldrIndexedSpec" $ do
    it "should be implemented" $ 0 `shouldBe` 1
