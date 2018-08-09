module BuildingsUnderConstructionSpec where

import qualified Data.PQueue.Min as PQ
import qualified Data.IntMap     as M

import BuildingsUnderConstruction
import Interpretor
import Coord
import Buildings

import Test.Hspec

spec :: Spec
spec =
  addBuildingSpec      >>
  tickConstructionSpec >>
  placeBuildingSpec

queueWithAnElement :: ConstructionQueue
queueWithAnElement = PQ.fromList [(3, (toCoord 2 4), Attack2)]

queueWithTwoElements :: ConstructionQueue
queueWithTwoElements = PQ.fromList [(0, (toCoord 2 4), Attack2), (3, (toCoord 2 4), Attack2)]

addBuildingSpec :: Spec
addBuildingSpec = do
  describe "addBuilding" $ do
    it "should add a building to an empty queue" $
      addBuilding (3, (toCoord 2 4), Attack2) PQ.empty `shouldBe` queueWithAnElement
    it "should add a building to a queue with elements in it" $
      addBuilding (0, (toCoord 2 4), Attack2) queueWithAnElement `shouldBe` queueWithTwoElements

tickConstructionSpec :: Spec
tickConstructionSpec = do
  describe "tickConstruction" $ do
    it "should produce an empty queue when given one" $
      tickConstruction PQ.empty `shouldBe` ([], PQ.empty)
    it "should decrement construction time and produce an empty list when no buildings get constructed" $
      tickConstruction queueWithAnElement
      `shouldBe`
      ([], PQ.fromList [(2, (toCoord 2 4), Attack2)])
    it "should produce a tuple containing the buildings which did finish and the queue with updated buildings" $
      tickConstruction queueWithTwoElements
      `shouldBe`
      ([(-1, (toCoord 2 4), Attack2)], PQ.fromList [(2, (toCoord 2 4), Attack2)])

placeBuildingSpec :: Spec
placeBuildingSpec = do
  describe "placeBuilding" $ do
    it "should place a building at an arbitrary point in the given map" $
      placeBuilding (0, (toCoord 2 4), Attack2) M.empty
      `shouldBe`
      M.fromList [((toCoord 2 4), Attack2)]
