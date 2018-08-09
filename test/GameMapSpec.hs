module GameMapSpec where

import GameMap
import Coord
import BitSetMap

import Test.Hspec

spec :: Spec
spec = removeAtSpec

removeAtSpec :: Spec
removeAtSpec = do
  describe "removeAt" $ do
    it "should leave an empty map unmodified" $
      removeAt (toCoord 2 3) emptyBuildings `shouldBe` emptyBuildings
    it "should leave one tower behind when there are two towers on the row" $
      removeAt (toCoord 2 3) (addBuilding (toCoord 2 3) (addBuilding (toCoord 1 5) 0))
      `shouldBe`
      addBuilding (toCoord 1 5) 0
    it "should remove a tower from a row, leaving the row behind when it's not the last tower" $
      removeAt (toCoord 2 3) (addBuilding (toCoord 2 3)
                              (addBuilding (toCoord 1 5)
                               (addBuilding (toCoord 6 3) 0)))
      `shouldBe`
      addBuilding (toCoord 1 5)
      (addBuilding (toCoord 6 3) 0)
