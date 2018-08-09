module MissileSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "moveMissile" $ do
    it "should be implemented" $ (1::Int) `shouldBe` 1
