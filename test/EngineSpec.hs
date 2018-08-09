module EngineSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "tickEngine" $ do
    it "should be implemented" $ (1::Int) `shouldBe` 1
