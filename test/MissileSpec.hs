module MissileSpec where

import Missile

import Test.Hspec

spec :: Spec
spec = do
  describe "moveMissile" $ do
    it "should be implemented" $ 1 `shouldBe` 1
