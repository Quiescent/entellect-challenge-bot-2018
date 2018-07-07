module BuildingsUnderConstructionSpec where

import qualified Data.PQueue.Min as PQ
import qualified Data.IntMap     as M

import BuildingsUnderConstruction
import Interpretor

import Test.Hspec

spec :: Spec
spec =
  addBuildingSpec      >>
  tickConstructionSpec >>
  placeBuildingSpec

anAttackTower :: Building
anAttackTower = (Building { integrity              = 5,
                            weaponCooldownTimeLeft = 2,
                            buildingType           = ATTACK })

anEnergyTower :: Building
anEnergyTower = (Building { integrity              = 5,
                            weaponCooldownTimeLeft = 0,
                            buildingType           = ENERGY })

aDefenseTower :: Building
aDefenseTower = (Building { integrity              = 20,
                            weaponCooldownTimeLeft = 0,
                            buildingType           = DEFENSE })

queueWithAnElement :: ConstructionQueue
queueWithAnElement = PQ.fromList [(3, (2, 4), anAttackTower)]

queueWithTwoElements :: ConstructionQueue
queueWithTwoElements = PQ.fromList [(0, (2, 4), anAttackTower), (3, (2, 4), anAttackTower)]

addBuildingSpec :: Spec
addBuildingSpec = do
  describe "addBuilding" $ do
    it "should add a building to an empty queue" $
      addBuilding (3, (2, 4), anAttackTower) PQ.empty `shouldBe` queueWithAnElement
    it "should add a building to a queue with elements in it" $
      addBuilding (0, (2, 4), anAttackTower) queueWithAnElement `shouldBe` queueWithTwoElements

tickConstructionSpec :: Spec
tickConstructionSpec = do
  describe "tickConstruction" $ do
    it "should produce an empty queue when given one" $
      tickConstruction PQ.empty `shouldBe` ([], PQ.empty)
    it "should decrement construction time and produce an empty list when no buildings get constructed" $
      tickConstruction queueWithAnElement
      `shouldBe`
      ([], PQ.fromList [(2, (2, 4), anAttackTower)])
    it "should produce a tuple containing the buildings which did finish and the queue with updated buildings" $
      tickConstruction queueWithTwoElements
      `shouldBe`
      ([(-1, (2, 4), anAttackTower)], PQ.fromList [(2, (2, 4), anAttackTower)])

placeBuildingSpec :: Spec
placeBuildingSpec = do
  describe "placeBuilding" $ do
    it "should place a building at an arbitrary point in the given map" $
      placeBuilding (0, (2, 4), anAttackTower) M.empty
      `shouldBe`
      M.fromList [(4, M.fromList [(2, anAttackTower)])]
