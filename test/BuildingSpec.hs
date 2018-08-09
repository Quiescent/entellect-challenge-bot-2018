module BuildingSpec where

import Building
import Interpretor
import Magic

import Test.Hspec

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
      emptyBoard { me = playerWithBuildingsInProgress { defenseTowersUnderConstruction2 = 0,
                                                        defenseTowersUnderConstruction1 = 1048576,
                                                        defenseTowersUnderConstruction0 = 16 } }
    it "should place buildings which finish construction" $
      tickBuildings boardWithBuildingsAboutToFinish
      `shouldBe`
      emptyBoard { me = emptyPlayer { energyTowers                    = 512,
                                      defense4Towers                  = 256,
                                      attack3Towers                   = 8796093022208,
                                      defenseTowersUnderConstruction1 = 1048576,
                                      defenseTowersUnderConstruction0 = 16,
                                      missiles0                       = 8796093022208 } }
    it "should update the cooldown of attack towers" $
      tickBuildings boardWithBuildingsOnIt
      `shouldBe`
      GameState { gameRound = 0,
                  me        = emptyPlayer { attack3Towers = 64321,
                                            attack2Towers = 234,
                                            attack1Towers = 87236,
                                            attack0Towers = 374564,
                                            missiles0     = 64321 },
                  oponent   = emptyPlayer { attack3Towers = 34684,
                                            attack2Towers = 6546874,
                                            attack1Towers = 651654,
                                            attack0Towers = 687643,
                                            missiles0     = 34684 } }

emptyPlayer :: Player
emptyPlayer =
  (Player  { energy                          = 37,
             energyGenPerTurn                = 0,
             energyTowersPerRow              = UV.fromList (replicate height 0),
             attackTowersPerRow              = UV.fromList (replicate height 0),
             health                          = 30,
             energyTowersUnderConstruction   = 0,
             energyTowers                    = 0,
             attackTowersUnderConstruction   = 0,
             attack3Towers                   = 0,
             attack2Towers                   = 0,
             attack1Towers                   = 0,
             attack0Towers                   = 0,
             defenseTowersUnderConstruction2 = 0,
             defenseTowersUnderConstruction1 = 0,
             defenseTowersUnderConstruction0 = 0,
             defense4Towers                  = 0,
             defense3Towers                  = 0,
             defense2Towers                  = 0,
             defense1Towers                  = 0,
             teslaTower0                     = 0,
             teslaTower1                     = 0,
             teslaTower0ConstructionTime     = 0,
             teslaTower1ConstructionTime     = 0,
             teslaTower0CooldownTime         = 0,
             teslaTower1CooldownTime         = 0,
             missiles0                       = 0,
             missiles1                       = 0,
             missiles2                       = 0,
             missiles3                       = 0 })

emptyBoard :: GameState
emptyBoard =
  GameState { gameRound = 0,
              me        = emptyPlayer,
              oponent   = emptyPlayer }

playerWithBuildingsInProgress :: Player
playerWithBuildingsInProgress =
  emptyPlayer { defenseTowersUnderConstruction2 = 1048576, -- 100000000000000000000
                defenseTowersUnderConstruction1 = 16 }     -- 10000

boardWithBuildingsInProgress :: GameState
boardWithBuildingsInProgress =
  GameState { gameRound = 0,
              me        = playerWithBuildingsInProgress,
              oponent   = emptyPlayer}

playerWithBuildingsAboutToFinish :: Player
playerWithBuildingsAboutToFinish =
  emptyPlayer { defenseTowersUnderConstruction2 = 1048576,        -- 100000000000000000000
                defenseTowersUnderConstruction1 = 16,             -- 10000
                defenseTowersUnderConstruction0 = 256,            -- 100000000
                energyTowersUnderConstruction   = 512,            -- 1000000000
                attackTowersUnderConstruction   = 8796093022208 } -- 10000000000000000000000000000000000000000000

boardWithBuildingsAboutToFinish :: GameState
boardWithBuildingsAboutToFinish =
  GameState { gameRound = 0,
              me        = playerWithBuildingsAboutToFinish,
              oponent   = emptyPlayer }

boardWithBuildingsOnIt :: GameState
boardWithBuildingsOnIt =
  GameState { gameRound = 0,
              me        = emptyPlayer { attack3Towers = 234,     -- 11101010
                                        attack2Towers = 87236,   -- 10101010011000100
                                        attack1Towers = 374564,  -- 1011011011100100100
                                        attack0Towers = 64321 }, -- 1111101101000001
              oponent   = emptyPlayer { attack3Towers = 6546874, -- 11000111110010110111010
                                        attack2Towers = 651654,  -- 10011111000110000110
                                        attack1Towers = 687643,  -- 10100111111000011011
                                        attack0Towers = 34684 }} -- 1000011101111100
