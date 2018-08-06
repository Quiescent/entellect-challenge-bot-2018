module EngineSpec where

import Test.Hspec

import Engine

spec :: Spec
spec = do
  describe "tickEngine" $ do
    it "should be implemented" $ 1 `shouldBe` 1
