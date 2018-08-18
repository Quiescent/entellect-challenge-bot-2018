module BuildingSpec where

import Building
import Interpretor
import BitSetMap
import Coord

import Test.Hspec

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
      emptyBoard { me = playerWithBuildingsInProgress { defenseTowersUnderConstruction2 = emptyBuildings,
                                                        defenseTowersUnderConstruction1 = addBuilding (toCoord 5 3) emptyBuildings,
                                                        defenseTowersUnderConstruction0 = addBuilding (toCoord 4 2) emptyBuildings } }
    it "should place buildings which finish construction" $
      tickBuildings boardWithBuildingsAboutToFinish
      `shouldBe`
      emptyBoard { me = playerWithBuildingsAboutToFinish
                   { allBuiltTowers                  = addBuilding (toCoord 1 3)
                                                       (addBuilding (toCoord 3 6)
                                                         (addBuilding (toCoord 3 2) emptyBuildings)),
                     energyTowersUnderConstruction   = emptyBuildings,
                     energyTowers                    = addBuilding (toCoord 3 6) emptyBuildings,
                     defense4Towers                  = addBuilding (toCoord 1 3) emptyBuildings,
                     attackTowersUnderConstruction   = emptyBuildings,
                     attack3Towers                   = addBuilding (toCoord 3 2) emptyBuildings,
                     defenseTowersUnderConstruction2 = emptyBuildings,
                     defenseTowersUnderConstruction1 = addBuilding (toCoord 5 3) emptyBuildings,
                     defenseTowersUnderConstruction0 = addBuilding (toCoord 4 2) emptyBuildings,
                     missiles0                       = addMissile (toCoord 3 2) emptyMissiles } }
    it "should update the cooldown of attack towers" $
      tickBuildings boardWithBuildingsOnIt
      `shouldBe`
      GameState { gameRound = 0,
                  me        = emptyPlayer { attack3Towers = addBuilding (toCoord 6 6) emptyBuildings,
                                            attack2Towers = addBuilding (toCoord 1 2) emptyBuildings,
                                            attack1Towers = addBuilding (toCoord 3 4) emptyBuildings,
                                            attack0Towers = addBuilding (toCoord 5 6) emptyBuildings,
                                            missiles0     = addMissile (toCoord 6 6) emptyMissiles },
                  oponent   = emptyPlayer { attack3Towers = addBuilding (toCoord 2 1) emptyBuildings,
                                            attack2Towers = addBuilding (toCoord 8 7) emptyBuildings,
                                            attack1Towers = addBuilding (toCoord 6 5) emptyBuildings,
                                            attack0Towers = addBuilding (toCoord 4 3) emptyBuildings,
                                            missiles0     = addMissile (toCoord 2 1) emptyMissiles} }
    it "should generate missiles which overlap with the first slot, in the second" $
      tickBuildings boardWithMissilesToBeGeneratedWithSomeOnExistingMissiles
      `shouldBe`
      GameState { gameRound = 0,
                  me = meWithMissilesToBeGeneratedWithSomeOnExistingMissiles
                         { attack0Towers = emptyBuildings,
                           attack3Towers = addBuilding (toCoord 3 2) emptyBuildings,
                           missiles0     = addMissile  (toCoord 3 2)
                                           (addMissile (toCoord 1 1) emptyMissiles),
                           missiles1     = addMissile  (toCoord 3 2) emptyMissiles },
                  oponent = oponentWithMissilesToBeGeneratedWithSomeOnExistingMissiles
                              { attack0Towers = 0,
                                attack3Towers = addBuilding (toCoord 1 2) emptyBuildings,
                                missiles0     = addMissile  (toCoord 5 2)
                                                (addMissile (toCoord 1 2) emptyMissiles),
                                missiles1     = addMissile (toCoord 1 2) emptyMissiles } }
    it "should bump all missiles to the second slot when the first is full" $
      tickBuildings boardWithMissilesToBeGeneratedAndBumpedToMissiles1
      `shouldBe`
      GameState { gameRound = 0,
                  me = meWithMissilesToBeGeneratedAndBumpedToMissiles1
                         { attack0Towers = emptyBuildings,
                           attack3Towers = addBuilding (toCoord 3 2) emptyBuildings,
                           missiles1     = addMissile (toCoord 3 2) emptyMissiles },
                  oponent = oponentWithMissilesToBeGeneratedAndBumpedToMissiles1
                              { attack0Towers = emptyBuildings,
                                attack3Towers = addBuilding (toCoord 1 2) emptyBuildings,
                                missiles1     = addMissile (toCoord 1 2) emptyMissiles } }
    it "should bump all missiles to the third slot when the first and second are full" $
      tickBuildings boardWithMissilesToBeGeneratedAndBumpedToMissiles2
      `shouldBe`
      GameState { gameRound = 0,
                  me = meWithMissilesToBeGeneratedAndBumpedToMissiles2
                         { attack0Towers = emptyBuildings,
                           attack3Towers = addBuilding (toCoord 3 2) emptyBuildings,
                           missiles2     = addMissile (toCoord 3 2) emptyMissiles },
                  oponent = oponentWithMissilesToBeGeneratedAndBumpedToMissiles2
                              { attack0Towers = emptyBuildings,
                                attack3Towers = addBuilding (toCoord 4 5) emptyBuildings,
                                missiles2     = addMissile (toCoord 4 5) emptyMissiles } }
    it "should bump all missiles to the third slot when the first and second are full" $
      tickBuildings boardWithMissilesToBeGeneratedAndBumpedToMissiles3
      `shouldBe`
      GameState { gameRound = 0,
                  me = meWithMissilesToBeGeneratedAndBumpedToMissiles3
                         { attack0Towers = emptyBuildings,
                           attack3Towers = addBuilding (toCoord 6 2) emptyBuildings,
                           missiles3     = addMissile (toCoord 6 2) emptyMissiles },
                  oponent = oponentWithMissilesToBeGeneratedAndBumpedToMissiles3
                              { attack0Towers = emptyBuildings,
                                attack3Towers = addBuilding (toCoord 3 2) emptyBuildings,
                                missiles3     = addMissile (toCoord 3 2) emptyMissiles } }

emptyPlayer :: Player
emptyPlayer =
  (Player  { energy                          = 37,
             health                          = 30,
             allTowers                       = emptyBuildings,
             allBuiltTowers                  = emptyBuildings,
             energyTowersUnderConstruction   = emptyBuildings,
             energyTowers                    = emptyBuildings,
             attackTowersUnderConstruction   = emptyBuildings,
             attack3Towers                   = emptyBuildings,
             attack2Towers                   = emptyBuildings,
             attack1Towers                   = emptyBuildings,
             attack0Towers                   = emptyBuildings,
             defenseTowersUnderConstruction2 = emptyBuildings,
             defenseTowersUnderConstruction1 = emptyBuildings,
             defenseTowersUnderConstruction0 = emptyBuildings,
             defense4Towers                  = emptyBuildings,
             defense3Towers                  = emptyBuildings,
             defense2Towers                  = emptyBuildings,
             defense1Towers                  = emptyBuildings,
             teslaTower0                     = emptyBuildings,
             teslaTower1                     = emptyBuildings,
             teslaTower0ConstructionTime     = 0,
             teslaTower1ConstructionTime     = 0,
             teslaTower0CooldownTime         = 0,
             teslaTower1CooldownTime         = 0,
             missiles0                       = emptyMissiles,
             missiles1                       = emptyMissiles,
             missiles2                       = emptyMissiles,
             missiles3                       = emptyMissiles,
             missilesOtherSide0              = emptyMissiles,
             missilesOtherSide1              = emptyMissiles,
             missilesOtherSide2              = emptyMissiles,
             missilesOtherSide3              = emptyMissiles })

emptyBoard :: GameState
emptyBoard =
  GameState { gameRound = 0,
              me        = emptyPlayer,
              oponent   = emptyPlayer }

playerWithBuildingsInProgress :: Player
playerWithBuildingsInProgress =
  emptyPlayer { allTowers                       = addBuilding (toCoord 4 2)
                                                  (addBuilding (toCoord 5 3) emptyBuildings),
                defenseTowersUnderConstruction2 = addBuilding (toCoord 5 3) emptyBuildings,
                defenseTowersUnderConstruction1 = addBuilding (toCoord 4 2) emptyBuildings }

boardWithBuildingsInProgress :: GameState
boardWithBuildingsInProgress =
  GameState { gameRound = 0,
              me        = playerWithBuildingsInProgress,
              oponent   = emptyPlayer}

playerWithBuildingsAboutToFinish :: Player
playerWithBuildingsAboutToFinish =
  emptyPlayer { allTowers                       = addBuilding (toCoord 4 2)
                                                  (addBuilding (toCoord 5 3)
                                                   (addBuilding (toCoord 3 3)
                                                    (addBuilding (toCoord 2 2)
                                                     (addBuilding (toCoord 4 3) emptyBuildings)))),
                defenseTowersUnderConstruction2 = addBuilding (toCoord 5 3) emptyBuildings,
                defenseTowersUnderConstruction1 = addBuilding (toCoord 4 2) emptyBuildings,
                defenseTowersUnderConstruction0 = addBuilding (toCoord 1 3) emptyBuildings,
                energyTowersUnderConstruction   = addBuilding (toCoord 3 6) emptyBuildings,
                attackTowersUnderConstruction   = addBuilding (toCoord 3 2) emptyBuildings } 

boardWithBuildingsAboutToFinish :: GameState
boardWithBuildingsAboutToFinish =
  GameState { gameRound = 0,
              me        = playerWithBuildingsAboutToFinish,
              oponent   = emptyPlayer }

boardWithBuildingsOnIt :: GameState
boardWithBuildingsOnIt =
  GameState { gameRound = 0,
              me        = emptyPlayer { attack3Towers = addBuilding (toCoord 1 2) emptyBuildings,
                                        attack2Towers = addBuilding (toCoord 3 4) emptyBuildings,
                                        attack1Towers = addBuilding (toCoord 5 6) emptyBuildings,
                                        attack0Towers = addBuilding (toCoord 6 6) emptyBuildings },
              oponent   = emptyPlayer { attack3Towers = addBuilding (toCoord 8 7) emptyBuildings,
                                        attack2Towers = addBuilding (toCoord 6 5) emptyBuildings,
                                        attack1Towers = addBuilding (toCoord 4 3) emptyBuildings,
                                        attack0Towers = addBuilding (toCoord 2 1) emptyBuildings }}

meWithMissilesToBeGeneratedWithSomeOnExistingMissiles :: Player
meWithMissilesToBeGeneratedWithSomeOnExistingMissiles =
  emptyPlayer { attack0Towers = addBuilding (toCoord 3 2) emptyBuildings,
                missiles0     = addMissile  (toCoord 3 2)
                                (addMissile (toCoord 1 1) emptyMissiles)}

oponentWithMissilesToBeGeneratedWithSomeOnExistingMissiles :: Player
oponentWithMissilesToBeGeneratedWithSomeOnExistingMissiles =
  emptyPlayer { attack0Towers = addBuilding (toCoord 1 2) emptyBuildings,
                missiles0     = addMissile  (toCoord 5 2)
                                (addMissile (toCoord 1 2) emptyMissiles)}

boardWithMissilesToBeGeneratedWithSomeOnExistingMissiles :: GameState
boardWithMissilesToBeGeneratedWithSomeOnExistingMissiles =
  GameState { gameRound = 0,
              me        = meWithMissilesToBeGeneratedWithSomeOnExistingMissiles,
              oponent   = oponentWithMissilesToBeGeneratedWithSomeOnExistingMissiles }

meWithMissilesToBeGeneratedAndBumpedToMissiles1 :: Player
meWithMissilesToBeGeneratedAndBumpedToMissiles1 =
  emptyPlayer { attack0Towers = addBuilding (toCoord 3 2) emptyBuildings,
                missiles0     = fullBoard }

oponentWithMissilesToBeGeneratedAndBumpedToMissiles1 :: Player
oponentWithMissilesToBeGeneratedAndBumpedToMissiles1 =
  emptyPlayer { attack0Towers =  addBuilding (toCoord 1 2) emptyBuildings,
                missiles0     =  fullBoard }

boardWithMissilesToBeGeneratedAndBumpedToMissiles1 :: GameState
boardWithMissilesToBeGeneratedAndBumpedToMissiles1 =
  GameState { gameRound = 0,
              me        = meWithMissilesToBeGeneratedAndBumpedToMissiles1,
              oponent   = oponentWithMissilesToBeGeneratedAndBumpedToMissiles1 }

meWithMissilesToBeGeneratedAndBumpedToMissiles2 :: Player
meWithMissilesToBeGeneratedAndBumpedToMissiles2 =
  emptyPlayer { attack0Towers = addBuilding (toCoord 3 2) emptyBuildings,
                missiles0     = fullBoard,
                missiles1     = fullBoard }

oponentWithMissilesToBeGeneratedAndBumpedToMissiles2 :: Player
oponentWithMissilesToBeGeneratedAndBumpedToMissiles2 =
  emptyPlayer { attack0Towers =  addBuilding (toCoord 4 5) emptyBuildings,
                missiles0     =  fullBoard,
                missiles1     =  fullBoard }

boardWithMissilesToBeGeneratedAndBumpedToMissiles2 :: GameState
boardWithMissilesToBeGeneratedAndBumpedToMissiles2 =
  GameState { gameRound = 0,
              me        = meWithMissilesToBeGeneratedAndBumpedToMissiles2,
              oponent   = oponentWithMissilesToBeGeneratedAndBumpedToMissiles2 }

meWithMissilesToBeGeneratedAndBumpedToMissiles3 :: Player
meWithMissilesToBeGeneratedAndBumpedToMissiles3 =
  emptyPlayer { attack0Towers = addBuilding (toCoord 6 2) emptyBuildings,
                missiles0     = fullBoard,
                missiles1     = fullBoard,
                missiles2     = fullBoard }

oponentWithMissilesToBeGeneratedAndBumpedToMissiles3 :: Player
oponentWithMissilesToBeGeneratedAndBumpedToMissiles3 =
  emptyPlayer { attack0Towers =  addBuilding (toCoord 3 2) emptyBuildings,
                missiles0     =  fullBoard,
                missiles1     =  fullBoard,
                missiles2     =  fullBoard }

boardWithMissilesToBeGeneratedAndBumpedToMissiles3 :: GameState
boardWithMissilesToBeGeneratedAndBumpedToMissiles3 =
  GameState { gameRound = 0,
              me        = meWithMissilesToBeGeneratedAndBumpedToMissiles3,
              oponent   = oponentWithMissilesToBeGeneratedAndBumpedToMissiles3 }
