module BuildingsUnderConstructionSpec where

import BuildingsUnderConstruction

import Test.Hspec

spec :: Spec
spec =
  addBuildingSpec                     >>
  createBuildingUnderConstructionSpec >>
  tickConstructionSpec                >>
  placeBuildingSpec

addBuildingSpec :: Spec
addBuildingSpec = do
  describe "addBuilding" $ do
    it "should be implemented" $ 0 `shouldBe` 1

createBuildingUnderConstructionSpec :: Spec
createBuildingUnderConstructionSpec = do
  describe "createBuildingUnderConstruction" $ do
    it "should be implemented" $ 0 `shouldBe` 1

tickConstructionSpec :: Spec
tickConstructionSpec = do
  describe "tickConstruction" $ do
    it "should be implemented" $ 0 `shouldBe` 1

placeBuildingSpec :: Spec
placeBuildingSpec = do
  describe "placeBuilding" $ do
    it "should be implemented" $ 0 `shouldBe` 1
