module GameMapSpec where

import GameMap
import Interpretor
import Magic
import Buildings
import Coord
import BitSetMap

import qualified Data.IntMap as M

import Test.Hspec

spec :: Spec
spec =
  removeAtSpec       >>
  findRightOfSpec    >>
  findLeftOfSpec

removeAtSpec :: Spec
removeAtSpec = do
  describe "removeAt" $ do
    it "should leave an empty map unmodified" $
      removeAt (toCoord 2 3) emptyBuildings `shouldBe` emptyBuildings
    it "should remove a row when it becomes empty" $
      removeAt (toCoord 2 3) ((addBuilding (toCoord 2 3) (addBuilding (toCoord 1 5) 0)))
      `shouldBe`
      addBuilding (toCoord 2 3) 0
    it "should remove a tower from a row, leaving the row behind when it's not the last tower" $
      removeAt (toCoord 2 3) (M.fromList [((toCoord 2 3), Attack2),
                                           ((toCoord 6 3), Attack2),
                                           ((toCoord 1 5), Attack2)])
      `shouldBe`
      M.fromList [((toCoord 6 3), Attack2),
                  ((toCoord 1 5), Attack2)]

findRightOfSpec :: Spec
findRightOfSpec = do
  describe "findRightOf" $ do
    it "should produce HitNothing given an empty map and a coordinate in the middle" $
      findRightOf (toCoord 2 5) M.empty `shouldBe` HitNothing
    it "should produce HitPlayer when given a coordinate whose x is -1" $
      findRightOf (toCoord (-1) 5) M.empty `shouldBe` HitPlayer
    it "should produce HitPlayer when given a coordinate whose x is -2" $
      findRightOf (toCoord (-2) 5) M.empty `shouldBe` HitPlayer
    -- TODO
    -- it "should produce HitBuilding when there's a building between it and the player" $
    --   findRightOf (toCoord (-1) 5) (M.fromList [((toCoord 0 5), Attack2)])
    --   `shouldBe`
    --   HitBuilding 0 Attack2
    it "should not produce HitBuilding when there's a building too far from it and the player" $
      findRightOf (toCoord (-1) 5) (M.fromList [((toCoord (missileSpeed + 1) 5), Attack2)])
      `shouldBe`
      HitPlayer

findLeftOfSpec :: Spec
findLeftOfSpec = do
  describe "findLeftOf" $ do
    it "should produce HitNothing given an empty map and a coordinate in the middle" $
      findLeftOf (toCoord 2 5) M.empty `shouldBe` HitNothing
    it "should produce HitPlayer when given a coordinate whose x is width" $
      findLeftOf (toCoord width 5) M.empty `shouldBe` HitPlayer
    it "should produce HitPlayer when given a coordinate whose x is width + 1" $
      findLeftOf (toCoord (width + 1) 5) M.empty `shouldBe` HitPlayer
    -- TODO
    -- it "should produce HitBuilding when there's a building between it and the player" $
    --   findLeftOf (toCoord width 5) (M.fromList [((toCoord 15 5), Attack2)])
    --   `shouldBe`
    --   HitBuilding 15 Attack2
    it "should not produce HitBuilding when there's a building too far from it and the player" $
      findLeftOf (toCoord width 5) (M.fromList [((toCoord (missileSpeed + 1) 5), Attack2)])
      `shouldBe`
      HitPlayer
