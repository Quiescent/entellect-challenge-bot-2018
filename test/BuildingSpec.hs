module BuildingSpec where

import Building
import Interpretor
import Magic

import Test.Hspec

import qualified Data.IntMap     as M
import qualified Data.PQueue.Min as PQ

spec :: Spec
spec = damageBuildingSpec >> tickBuildingSpec

anAttackTower :: Building
anAttackTower = (Building { integrity              = 5,
                            weaponCooldownTimeLeft = 0,
                            buildingType           = ATTACK })

anAttackTowerWithResetCooldown :: Building
anAttackTowerWithResetCooldown = (Building { integrity              = 5,
                                             weaponCooldownTimeLeft = attackTowerCooldownTime,
                                             buildingType           = ATTACK })

damageBuildingSpec :: Spec
damageBuildingSpec = do
  describe "damageBuilding" $ do
    it "should do nothing to a building when the damage is zero" $
      damageBuilding 0 anAttackTower `shouldBe` Just anAttackTower
    it "should remove the damage amount from the health of a building" $
      damageBuilding 2 anAttackTower `shouldBe` Just (anAttackTower { integrity = 3 })
    it "should produce nothing when damaged to zero" $
      damageBuilding 5 anAttackTower `shouldBe` Nothing

tickBuildingSpec :: Spec
tickBuildingSpec = do
  describe "tickBuildings" $ do
    it "should, given two players with empty maps, do nothing" $
      tickBuildings emptyBoard `shouldBe` emptyBoard
    it "should advance the construction state of buildings on the queue" $
      tickBuildings boardWithBuildingsInProgress
      `shouldBe`
      emptyBoard { me = playerWithBuildingsInProgress { constructionQueue =
                                                         PQ.fromList [(1, (0, 0), anAttackTower),
                                                                      (2, (2, 5), anAttackTower)]}  }
    it "should place buildings which finish construction" $
      tickBuildings boardWithBuildingsAboutToFinish
      `shouldBe`
      emptyBoard { me = emptyPlayer { towerMap = M.fromList [(0, M.fromList [(0, anAttackTowerWithResetCooldown)]),
                                                             (5, M.fromList [(2, anAttackTowerWithResetCooldown)])],
                                      ownedMissiles = [(Missile 0 0), (Missile 2 5)]} }
    it "should update the cooldown of attack towers" $
      tickBuildings boardWithBuildingsOnIt
      `shouldBe`
      GameState { me      = emptyPlayer { towerMap = M.fromList [(1, M.fromList [(2,  anAttackTowerWithUpdatedCooldown),
                                                                                 (5,  anAttackTowerWithUpdatedCooldown)])] },
                  oponent = emptyPlayer { towerMap = M.fromList [(3, M.fromList [(8,  anAttackTowerWithUpdatedCooldown),
                                                                                 (14, anAttackTowerWithUpdatedCooldown)])] }}

emptyPlayer :: Player
emptyPlayer =
  (Player { energy            = 37,
            health            = 30,
            hitsTaken         = 14,
            score             = 451,
            towerMap          = M.empty,
            constructionQueue = PQ.empty,
            ownedMissiles     = []})

emptyBoard :: GameState
emptyBoard =
  GameState { me      = emptyPlayer,
              oponent = emptyPlayer }

playerWithBuildingsInProgress :: Player
playerWithBuildingsInProgress =
  emptyPlayer { constructionQueue = PQ.fromList [(2, (0, 0), anAttackTower),
                                                 (3, (2, 5), anAttackTower)] }

boardWithBuildingsInProgress :: GameState
boardWithBuildingsInProgress =
  GameState { me      = playerWithBuildingsInProgress,
              oponent = emptyPlayer}

playerWithBuildingsAboutToFinish :: Player
playerWithBuildingsAboutToFinish =
  emptyPlayer { constructionQueue = PQ.fromList [(0, (0, 0), anAttackTower),
                                                 (0, (2, 5), anAttackTower)] }

boardWithBuildingsAboutToFinish :: GameState
boardWithBuildingsAboutToFinish =
  GameState { me      = playerWithBuildingsAboutToFinish,
              oponent = emptyPlayer }

boardWithBuildingsOnIt :: GameState
boardWithBuildingsOnIt =
  GameState { me      = emptyPlayer { towerMap = M.fromList [(1, M.fromList [(2, anAttackTowerWithCooldown),
                                                                             (5, anAttackTowerWithCooldown)])] },
              oponent = emptyPlayer { towerMap = M.fromList [(3, M.fromList [(8, anAttackTowerWithCooldown),
                                                                             (14, anAttackTowerWithCooldown)])] }}
anAttackTowerWithCooldown :: Building
anAttackTowerWithCooldown = (Building { integrity              = 5,
                                        weaponCooldownTimeLeft = 3,
                                        buildingType           = ATTACK })

anAttackTowerWithUpdatedCooldown :: Building
anAttackTowerWithUpdatedCooldown = (Building { integrity              = 5,
                                               weaponCooldownTimeLeft = 2,
                                               buildingType           = ATTACK })
