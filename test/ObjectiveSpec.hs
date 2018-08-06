module ObjectiveSpec where

import Objective

import Test.Hspec

spec :: Spec
spec = do
  describe "myBoardScore" $ do
    it "should be implemented" $ 1 `shouldBe` 1
