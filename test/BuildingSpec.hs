module BuildingSpec where

import Building
import Interpretor
import Magic
import Buildings
import Coord

import Test.Hspec

import qualified Data.IntMap         as M
import qualified Data.PQueue.Min     as PQ
import qualified Data.Vector.Unboxed as UV

spec :: Spec
spec = damageBuildingSpec >> tickBuildingSpec

damageBuildingSpec :: Spec
damageBuildingSpec = do
  describe "damageBuilding" $ do
    it "should remove the damage amount from the health of a building" $
      missileDamagesBuilding defense4 `shouldBe` Just defense3
    it "should produce nothing when damaged to zero" $
      missileDamagesBuilding attack3 `shouldBe` Nothing

tickBuildingSpec :: Spec
tickBuildingSpec = do
  describe "tickBuildings" $ do
    it "should, given two players with empty maps, do nothing" $
      tickBuildings emptyBoard `shouldBe` emptyBoard
    it "should advance the construction state of buildings on the queue" $
      tickBuildings boardWithBuildingsInProgress
      `shouldBe`
      emptyBoard { me = playerWithBuildingsInProgress { constructionQueue =
                                                         PQ.fromList [(1, (toCoord 0 0), attack3),
                                                                      (2, (toCoord 2 5), attack3)]}  }
    it "should place buildings which finish construction" $
      tickBuildings boardWithBuildingsAboutToFinish
      `shouldBe`
      emptyBoard { me = emptyPlayer { towerMap = M.fromList [((toCoord 0 0), attack3),
                                                             ((toCoord 2 5), attack3)],
                                      ownedMissiles = UV.fromList [(toCoord 0 0), (toCoord 2 5)]} }
    it "should update the cooldown of attack towers" $
      tickBuildings boardWithBuildingsOnIt
      `shouldBe`
      GameState { me      = emptyPlayer { towerMap = M.fromList [((toCoord 2 1),   attack2),
                                                                  ((toCoord 5 1),  attack2)] },
                  oponent = emptyPlayer { towerMap = M.fromList [((toCoord 8 3),   attack2),
                                                                  ((toCoord 14 3), attack2)] }}

emptyPlayer :: Player
emptyPlayer =
  (Player { energy            = 37,
            energyGenPerTurn  = 0,
            health            = 30,
            towerMap          = M.empty,
            constructionQueue = PQ.empty,
            ownedMissiles     = UV.empty })

emptyBoard :: GameState
emptyBoard =
  GameState { me      = emptyPlayer,
              oponent = emptyPlayer }

playerWithBuildingsInProgress :: Player
playerWithBuildingsInProgress =
  emptyPlayer { constructionQueue = PQ.fromList [(2, (toCoord 0 0), attack3),
                                                 (3, (toCoord 2 5), attack3)] }

boardWithBuildingsInProgress :: GameState
boardWithBuildingsInProgress =
  GameState { me      = playerWithBuildingsInProgress,
              oponent = emptyPlayer}

playerWithBuildingsAboutToFinish :: Player
playerWithBuildingsAboutToFinish =
  emptyPlayer { constructionQueue = PQ.fromList [(0, (toCoord 0 0), attack0),
                                                 (0, (toCoord 2 5), attack0)] }

boardWithBuildingsAboutToFinish :: GameState
boardWithBuildingsAboutToFinish =
  GameState { me      = playerWithBuildingsAboutToFinish,
              oponent = emptyPlayer }

boardWithBuildingsOnIt :: GameState
boardWithBuildingsOnIt =
  GameState { me      = emptyPlayer { towerMap = M.fromList [((toCoord 2 1), attack3),
                                                             ((toCoord 5 1), attack3)] },
              oponent = emptyPlayer { towerMap = M.fromList [((toCoord 8 3), attack3),
                                                             ((toCoord 14 3), attack3)] }}