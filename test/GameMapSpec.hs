module GameMapSpec where

import GameMap
import Interpretor
import Magic

import qualified Data.IntMap as M

import Test.Hspec

spec :: Spec
spec =
  removeAtSpec       >>
  findRightOfSpec    >>
  findLeftOfSpec

anAttackTower :: Building
anAttackTower = (Building { integrity              = 5,
                            weaponCooldownTimeLeft = 2,
                            buildingType           = ATTACK })

removeAtSpec :: Spec
removeAtSpec = do
  describe "removeAt" $ do
    it "should leave an empty map unmodified" $
      removeAt (2, 3) M.empty `shouldBe` M.empty
    it "should remove a row when it becomes empty" $
      removeAt (2, 3) (M.fromList [(3, M.fromList [(2, anAttackTower)]),
                                   (5, M.fromList [(1, anAttackTower)])])
      `shouldBe`
      M.fromList [(5, M.fromList [(1, anAttackTower)])]
    it "should remove a tower from a row, leaving the row behind when it's not the last tower" $
      removeAt (2, 3) (M.fromList [(3, M.fromList [(2, anAttackTower),
                                                   (6, anAttackTower)]),
                                   (5, M.fromList [(1, anAttackTower)])])
      `shouldBe`
      M.fromList [(3, M.fromList [(6, anAttackTower)]),
                  (5, M.fromList [(1, anAttackTower)])]

findRightOfSpec :: Spec
findRightOfSpec = do
  describe "findRightOf" $ do
    it "should produce HitNothing given an empty map and a coordinate in the middle" $
      findRightOf (2, 5) M.empty `shouldBe` HitNothing
    it "should produce HitPlayer when given a coordinate whose x is -1" $
      findRightOf (-1, 5) M.empty `shouldBe` HitPlayer
    it "should produce HitPlayer when given a coordinate whose x is -2" $
      findRightOf (-2, 5) M.empty `shouldBe` HitPlayer
    it "should produce HitBuilding when there's a building between it and the player" $
      findRightOf (-1, 5) (M.fromList [(5, M.fromList [(0, anAttackTower)])])
      `shouldBe`
      HitBuilding 0 anAttackTower
    it "should not produce HitBuilding when there's a building too far from it and the player" $
      findRightOf (-1, 5) (M.fromList [(5, M.fromList [(missileSpeed + 1, anAttackTower)])])
      `shouldBe`
      HitPlayer

findLeftOfSpec :: Spec
findLeftOfSpec = do
  describe "findLeftOf" $ do
    it "should produce HitNothing given an empty map and a coordinate in the middle" $
      findLeftOf (2, 5) M.empty `shouldBe` HitNothing
    it "should produce HitPlayer when given a coordinate whose x is width" $
      findLeftOf (width, 5) M.empty `shouldBe` HitPlayer
    it "should produce HitPlayer when given a coordinate whose x is width + 1" $
      findLeftOf (width + 1, 5) M.empty `shouldBe` HitPlayer
    it "should produce HitBuilding when there's a building between it and the player" $
      findLeftOf (width, 5) (M.fromList [(5, M.fromList [(15, anAttackTower)])])
      `shouldBe`
      HitBuilding 15 anAttackTower
    it "should not produce HitBuilding when there's a building too far from it and the player" $
      findLeftOf (width, 5) (M.fromList [(5, M.fromList [(missileSpeed + 1, anAttackTower)])])
      `shouldBe`
      HitPlayer
