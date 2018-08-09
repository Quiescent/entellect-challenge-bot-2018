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
spec = tickBuildingSpec

tickBuildingSpec :: Spec
tickBuildingSpec = do
  describe "tickBuildings" $ do
    it "should, given two players with empty maps, do nothing" $
      tickBuildings emptyBoard `shouldBe` emptyBoard
    it "should advance the construction state of buildings on the queue" $
      tickBuildings boardWithBuildingsInProgress
      `shouldBe`
      emptyBoard { me = playerWithBuildingsInProgress { constructionQueue =
                                                         PQ.fromList [(1, (toCoord 0 0), Attack3),
                                                                      (2, (toCoord 2 5), Attack3)]}  }
    it "should place buildings which finish construction" $
      tickBuildings boardWithBuildingsAboutToFinish
      `shouldBe`
      emptyBoard { me = emptyPlayer { towerMap = M.fromList [((toCoord 0 0), Attack3),
                                                             ((toCoord 2 5), Attack3)],
                                      ownedMissiles = UV.fromList [(toCoord 0 0), (toCoord 2 5)]} }
    it "should update the cooldown of attack towers" $
      tickBuildings boardWithBuildingsOnIt
      `shouldBe`
      GameState { gameRound = 0,
                  me        = emptyPlayer { towerMap = M.fromList [((toCoord 2 1),   Attack2),
                                                                    ((toCoord 5 1),  Attack2)] },
                  oponent   = emptyPlayer { towerMap = M.fromList [((toCoord 8 3),   Attack2),
                                                                    ((toCoord 14 3), Attack2)] }}

emptyPlayer :: Player
emptyPlayer =
  (Player  { energy             = 37,
             energyGenPerTurn   = 0,
             energyTowersPerRow = UV.fromList (replicate height 0),
             attackTowersPerRow = UV.fromList (replicate height 0),
             health             = 30,
             towerMap           = M.empty,
             constructionQueue  = PQ.empty,
             ownedMissiles      = UV.empty })

emptyBoard :: GameState
emptyBoard =
  GameState { gameRound = 0,
              me        = emptyPlayer,
              oponent   = emptyPlayer }

playerWithBuildingsInProgress :: Player
playerWithBuildingsInProgress =
  emptyPlayer { constructionQueue = PQ.fromList [(2, (toCoord 0 0), Attack3),
                                                 (3, (toCoord 2 5), Attack3)] }

boardWithBuildingsInProgress :: GameState
boardWithBuildingsInProgress =
  GameState { gameRound = 0,
              me        = playerWithBuildingsInProgress,
              oponent   = emptyPlayer}

playerWithBuildingsAboutToFinish :: Player
playerWithBuildingsAboutToFinish =
  emptyPlayer { constructionQueue = PQ.fromList [(0, (toCoord 0 0), Attack0),
                                                 (0, (toCoord 2 5), Attack0)] }

boardWithBuildingsAboutToFinish :: GameState
boardWithBuildingsAboutToFinish =
  GameState { gameRound = 0,
              me        = playerWithBuildingsAboutToFinish,
              oponent   = emptyPlayer }

boardWithBuildingsOnIt :: GameState
boardWithBuildingsOnIt =
  GameState { gameRound = 0,
              me        = emptyPlayer { towerMap = M.fromList [((toCoord 2 1), Attack3),
                                                               ((toCoord 5 1), Attack3)] },
              oponent   = emptyPlayer { towerMap = M.fromList [((toCoord 8 3), Attack3),
                                                               ((toCoord 14 3), Attack3)] }}
