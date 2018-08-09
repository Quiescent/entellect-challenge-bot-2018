module ObjectiveSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "myBoardScore" $ do
    it "should be implemented" $ (1::Int) `shouldBe` 1
