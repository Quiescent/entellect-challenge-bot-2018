module GameMapSpec where

import Test.Hspec

import GameMap

spec :: Spec
spec =
  mapFoldSpec        >>
  removeAtSpec       >>
  mapFoldIndexedSpec >>
  adjustAtSpec       >>
  replaceAtSpec      >>
  definedAtSpec      >>
  addAtSpec          >>
  findRightOfSpec    >>
  findLeftOfSpec

mapFoldSpec :: Spec
mapFoldSpec = do
  describe "mapFold" $ do
    it "should be implemented" $ 0 `shouldBe` 1

removeAtSpec :: Spec
removeAtSpec = do
  describe "removeAt" $ do
    it "should be implemented" $ 0 `shouldBe` 1

mapFoldIndexedSpec :: Spec
mapFoldIndexedSpec = do
  describe "mapFoldIndexed" $ do
    it "should be implemented" $ 0 `shouldBe` 1

adjustAtSpec :: Spec
adjustAtSpec = do
  describe "adjustAt" $ do
    it "should be implemented" $ 0 `shouldBe` 1

replaceAtSpec :: Spec
replaceAtSpec = do
  describe "replaceAt" $ do
    it "should be implemented" $ 0 `shouldBe` 1

definedAtSpec :: Spec
definedAtSpec = do
  describe "definedAt" $ do
    it "should be implemented" $ 0 `shouldBe` 1

addAtSpec :: Spec
addAtSpec = do
  describe "addAt" $ do
    it "should be implemented" $ 0 `shouldBe` 1

findRightOfSpec :: Spec
findRightOfSpec = do
  describe "findRightOf" $ do
    it "should be implemented" $ 0 `shouldBe` 1

findLeftOfSpec :: Spec
findLeftOfSpec = do
  describe "findLeftOf" $ do
    it "should be implemented" $ 0 `shouldBe` 1
