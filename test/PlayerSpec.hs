module PlayerSpec where

import Player
import Interpretor
import Buildings
import Coord
import EfficientCommand
import Magic
import BitSetMap

import qualified Data.Vector.Unboxed as UV

import Test.Hspec

spec :: Spec
spec =
  updateEnergySpec           >>
  myPlayerSpec               >>
  oponentsPlayerSpec         >>
  myEnergySpec               >>
  oponentsEnergySpec         >>
  myHealthSpec               >>
  oponentsHealthSpec         >>
  takeDamageSpec             >>
  buildingFromStatsSpec      >>
  updateMoveSpec             >>
  deconstructAtSpec          >>
  collideSpec                >>
  moveCheckingBoundariesSpec

aPlayer :: Player
aPlayer = (Player { energy                          = 0,
                    health                          = 25,
                    energyGenPerTurn                = 0,
                    energyTowersPerRow              = UV.fromList (replicate height 0),
                    attackTowersPerRow              = UV.fromList (replicate height 0),
                    energyTowersUnderConstruction   = 0,
                    energyTowers                    = addMissile (toCoord 3 4) 0,
                    attackTowersUnderConstruction   = 0,
                    attack3Towers                   = addMissile (toCoord 4 4) 0,
                    attack2Towers                   = addMissile (toCoord 5 4) 0,
                    attack1Towers                   = addMissile (toCoord 6 4) 0,
                    attack0Towers                   = addMissile (toCoord 7 4) 0,
                    defenseTowersUnderConstruction2 = 0,
                    defenseTowersUnderConstruction1 = 0,
                    defenseTowersUnderConstruction0 = 0,
                    defense4Towers                  = addMissile (toCoord 3 5) 0,
                    defense3Towers                  = addMissile (toCoord 3 6) 0,
                    defense2Towers                  = addMissile (toCoord 3 7) 0,
                    defense1Towers                  = addMissile (toCoord 5 5) 0,
                    teslaTower0                     = 0,
                    teslaTower1                     = 0,
                    teslaTower0ConstructionTime     = 0,
                    teslaTower1ConstructionTime     = 0,
                    teslaTower0CooldownTime         = 0,
                    teslaTower1CooldownTime         = 0,
                    missiles0                       = 0,
                    missiles1                       = 0,
                    missiles2                       = 0,
                    missiles3                       = 0,
                    missilesOtherSide0              = 0,
                    missilesOtherSide1              = 0,
                    missilesOtherSide2              = 0,
                    missilesOtherSide3              = 0 })

aPlayerWithNonZeroEnergy :: Player
aPlayerWithNonZeroEnergy = (Player { energy                          = 10,
                                     health                          = 15,
                                     energyGenPerTurn                = 0,
                                     energyTowersPerRow              = UV.fromList (replicate height 0),
                                     attackTowersPerRow              = UV.fromList (replicate height 0),
                                     energyTowersUnderConstruction   = 0,
                                     energyTowers                    = addMissile (toCoord 3 4)
                                                                       (addMissile (toCoord 0 3) 0),
                                     attackTowersUnderConstruction   = 0,
                                     attack3Towers                   = addMissile (toCoord 4 4)
                                                                       (addMissile (toCoord 1 3) 0),
                                     attack2Towers                   = addMissile (toCoord 5 4)
                                                                       (addMissile (toCoord 2 3) 0),
                                     attack1Towers                   = addMissile (toCoord 6 4)
                                                                       (addMissile (toCoord 3 3) 0),
                                     attack0Towers                   = addMissile (toCoord 7 4)
                                                                       (addMissile (toCoord 4 3) 0),
                                     defenseTowersUnderConstruction2 = 0,
                                     defenseTowersUnderConstruction1 = 0,
                                     defenseTowersUnderConstruction0 = 0,
                                     defense4Towers                  = addMissile (toCoord 3 5)
                                                                       (addMissile (toCoord 5 3) 0),
                                     defense3Towers                  = addMissile (toCoord 3 6)
                                                                       (addMissile (toCoord 6 3) 0),
                                     defense2Towers                  = addMissile (toCoord 3 7)
                                                                       (addMissile (toCoord 7 3) 0),
                                     defense1Towers                  = addMissile (toCoord 5 5)
                                                                       (addMissile (toCoord 0 4) 0),
                                     teslaTower0                     = 0,
                                     teslaTower1                     = 0,
                                     teslaTower0ConstructionTime     = 0,
                                     teslaTower1ConstructionTime     = 0,
                                     teslaTower0CooldownTime         = 0,
                                     teslaTower1CooldownTime         = 0,
                                     missiles0                       = 0,
                                     missiles1                       = 0,
                                     missiles2                       = 0,
                                     missiles3                       = 0,
                                     missilesOtherSide0              = 0,
                                     missilesOtherSide1              = 0,
                                     missilesOtherSide2              = 0,
                                     missilesOtherSide3              = 0 })

aGameState :: GameState
aGameState = (GameState { gameRound = 0,
                          me        = aPlayer,
                          oponent   = aPlayerWithNonZeroEnergy })

updateEnergySpec :: Spec
updateEnergySpec = do
  describe "updateEnergy" $ do
    it "should set the players energy when it's zero" $
      (updateEnergy 10 aPlayer) `shouldBe` (aPlayer { energy = 10 })
    it "should add to the players energy when it's non-zero" $
      (updateEnergy 10 aPlayerWithNonZeroEnergy) `shouldBe` (aPlayerWithNonZeroEnergy { energy = 20 })

myPlayerSpec :: Spec
myPlayerSpec = do
  describe "myPlayer" $ do
    it "should select `me' from game states" $
      (myPlayer aGameState) `shouldBe` aPlayer

oponentsPlayerSpec :: Spec
oponentsPlayerSpec = do
  describe "oponentsPlayer" $ do
    it "should select `oponent' from game states" $
      (oponentsPlayer aGameState) `shouldBe` aPlayerWithNonZeroEnergy

myEnergySpec :: Spec
myEnergySpec = do
  describe "myEnergy" $ do
    it "should produce the energy of `me' from game states" $
      (myEnergy aGameState) `shouldBe` 0

oponentsEnergySpec :: Spec
oponentsEnergySpec = do
  describe "oponentsEnergy" $ do
    it "should produce the energy of `oponent' from game states" $
      (oponentsEnergy aGameState) `shouldBe` 10

myHealthSpec :: Spec
myHealthSpec = do
  describe "myHealth" $ do
    it "should produce the health of `me' from game states" $
      (myHealth aGameState) `shouldBe` 25

oponentsHealthSpec :: Spec
oponentsHealthSpec = do
  describe "oponentsHealth" $ do
    it "should produce the health of `oponent' from game states" $
      (oponentsHealth aGameState) `shouldBe` 15

takeDamageSpec :: Spec
takeDamageSpec = do
  describe "takeDamage" $ do
    it "should produce a player with negative health when taking enough damage" $
      (takeDamage 35 aPlayer) `shouldBe` (aPlayer { health = -10 })
    it "should produce a player with non-negative health when given a playre with non-zero health above damage" $
      (takeDamage 10 aPlayerWithNonZeroEnergy) `shouldBe` (aPlayerWithNonZeroEnergy { health = 5 })

buildingFromStatsSpec :: Spec
buildingFromStatsSpec = do
  describe "buildingFromStats" $ do
    it "should produce a building with the given building statistics" $
      (buildingFromStats ATTACK) `shouldBe` Attack0

deconstructAtSpec :: Spec
deconstructAtSpec = do
  describe "deconstructAt" $ do
    it "should remove an energy tower at the given coordinates" $
      (deconstructAt (toCoord 3 4) aPlayer) `shouldBe` (aPlayer { energyTowers = 0 })
    it "energy: should not remove anything else when removing that tower" $
      (deconstructAt (toCoord 3 4) aPlayerWithNonZeroEnergy)
      `shouldBe`
      (aPlayerWithNonZeroEnergy { energyTowers = addMissile (toCoord 0 3) 0 })
    it "should remove an attack3 tower at the given coordinates" $
      (deconstructAt (toCoord 4 4) aPlayer) `shouldBe` (aPlayer { attack3Towers = 0 })
    it "attack3: should not remove anything else when removing that tower" $
      (deconstructAt (toCoord 4 4) aPlayerWithNonZeroEnergy)
      `shouldBe`
      (aPlayerWithNonZeroEnergy { attack3Towers = addMissile (toCoord 1 3) 0 })
    it "should remove an attack2 tower at the given coordinates" $
      (deconstructAt (toCoord 5 4) aPlayer) `shouldBe` (aPlayer { attack2Towers = 0 })
    it "attack2: should not remove anything else when removing that tower" $
      (deconstructAt (toCoord 5 4) aPlayerWithNonZeroEnergy)
      `shouldBe`
      (aPlayerWithNonZeroEnergy { attack2Towers = addMissile (toCoord 2 3) 0 })
    it "should remove an attack1 tower at the given coordinates" $
      (deconstructAt (toCoord 6 4) aPlayer) `shouldBe` (aPlayer { attack1Towers = 0 })
    it "attack1: should not remove anything else when removing that tower" $
      (deconstructAt (toCoord 6 4) aPlayerWithNonZeroEnergy)
      `shouldBe`
      (aPlayerWithNonZeroEnergy { attack1Towers = addMissile (toCoord 3 3) 0 })
    it "should remove an attack0 tower at the given coordinates" $
      (deconstructAt (toCoord 7 4) aPlayer) `shouldBe` (aPlayer { attack0Towers = 0 })
    it "attack0: should not remove anything else when removing that tower" $
      (deconstructAt (toCoord 7 4) aPlayerWithNonZeroEnergy)
      `shouldBe`
      (aPlayerWithNonZeroEnergy { attack0Towers = addMissile (toCoord 4 3) 0 })
    it "should remove an defense4 tower at the given coordinates" $
      (deconstructAt (toCoord 3 5) aPlayer) `shouldBe` (aPlayer { defense4Towers = 0 })
    it "defense4: should not remove anything else when removing that tower" $
      (deconstructAt (toCoord 3 5) aPlayerWithNonZeroEnergy)
      `shouldBe`
      (aPlayerWithNonZeroEnergy { defense4Towers = addMissile (toCoord 5 3) 0 })
    it "should remove an defense3 tower at the given coordinates" $
      (deconstructAt (toCoord 3 6) aPlayer) `shouldBe` (aPlayer { defense3Towers = 0 })
    it "defense3: should not remove anything else when removing that tower" $
      (deconstructAt (toCoord 3 6) aPlayerWithNonZeroEnergy)
      `shouldBe`
      (aPlayerWithNonZeroEnergy { defense3Towers = addMissile (toCoord 6 3) 0 })
    it "should remove an defense2 tower at the given coordinates" $
      (deconstructAt (toCoord 3 7) aPlayer) `shouldBe` (aPlayer { defense2Towers = 0 })
    it "defense2: should not remove anything else when removing that tower" $
      (deconstructAt (toCoord 3 7) aPlayerWithNonZeroEnergy)
      `shouldBe`
      (aPlayerWithNonZeroEnergy { defense2Towers = addMissile (toCoord 7 3) 0 })
    it "should remove an defense1 tower at the given coordinates" $
      (deconstructAt (toCoord 5 5) aPlayer) `shouldBe` (aPlayer { defense1Towers = 0 })
    it "defense1: should not remove anything else when removing that tower" $
      (deconstructAt (toCoord 5 5) aPlayerWithNonZeroEnergy)
      `shouldBe`
      (aPlayerWithNonZeroEnergy { defense1Towers = addMissile (toCoord 0 4) 0 })

aPlayerWithAnEnergyTowerAtTwoTwo :: Player
aPlayerWithAnEnergyTowerAtTwoTwo =
  Player { energy                          = 0,
           health                          = 0,
           energyGenPerTurn                = 0,
           energyTowersPerRow              = UV.empty,
           attackTowersPerRow              = UV.empty,
           energyTowersUnderConstruction   = 0,
           energyTowers                    = addBuilding (toCoord 2 2) 0,
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
           missiles3                       = 0,
           missilesOtherSide0              = 0,
           missilesOtherSide1              = 0,
           missilesOtherSide2              = 0,
           missilesOtherSide3              = 0 }

aPlayerWithAMissileOnTheOtherSideAtTwoTwo :: Player
aPlayerWithAMissileOnTheOtherSideAtTwoTwo =
  Player { energy                          = 0,
           health                          = 0,
           energyGenPerTurn                = 0,
           energyTowersPerRow              = UV.empty,
           attackTowersPerRow              = UV.empty,
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
           missiles3                       = 0,
           missilesOtherSide0              = addMissile (toCoord 2 2) 0,
           missilesOtherSide1              = 0,
           missilesOtherSide2              = 0,
           missilesOtherSide3              = 0 }

aPlayerWithAMissileOnTwoTwo :: Player
aPlayerWithAMissileOnTwoTwo =
  Player { energy                          = 0,
           health                          = 0,
           energyGenPerTurn                = 0,
           energyTowersPerRow              = UV.empty,
           attackTowersPerRow              = UV.empty,
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
           missiles0                       = addMissile (toCoord 2 2) 0,
           missiles1                       = 0,
           missiles2                       = 0,
           missiles3                       = 0,
           missilesOtherSide0              = 0,
           missilesOtherSide1              = 0,
           missilesOtherSide2              = 0,
           missilesOtherSide3              = 0 }

anEmptyPlayer :: Player
anEmptyPlayer = Player { energy                          = 0,
                         health                          = 0,
                         energyGenPerTurn                = 0,
                         energyTowersPerRow              = UV.empty,
                         attackTowersPerRow              = UV.empty,
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
                         missiles3                       = 0,
                         missilesOtherSide0              = 0,
                         missilesOtherSide1              = 0,
                         missilesOtherSide2              = 0,
                         missilesOtherSide3              = 0 }

collideSpec :: Spec
collideSpec = do
  describe "collide" $ do
    it "should remove an energy tower" $
      collide aPlayerWithAMissileOnTheOtherSideAtTwoTwo aPlayerWithAnEnergyTowerAtTwoTwo
      `shouldBe`
      (anEmptyPlayer, anEmptyPlayer)
    it "shouldn't do anything when the missiles don't line up" $
      collide aPlayerWithAMissileOnTheOtherSideAtTwoTwo anEmptyPlayer
      `shouldBe`
      (aPlayerWithAMissileOnTheOtherSideAtTwoTwo, anEmptyPlayer)
    it "shouldn't remove an energy tower which no missiles collided with" $
      collide aPlayerWithAMissileOnTheOtherSideAtTwoTwo
              (aPlayerWithAMissileOnTheOtherSideAtTwoTwo
                 { energyTowers = addBuilding (toCoord 2 2) (addBuilding (toCoord 3 4) 0) })
      `shouldBe`
      (anEmptyPlayer, aPlayerWithAMissileOnTheOtherSideAtTwoTwo {
          energyTowers = addBuilding (toCoord 3 4) 0 })

aPlayerWithAMissileOnSevenTwo :: Player
aPlayerWithAMissileOnSevenTwo =
  anEmptyPlayer { missiles0 = addMissile (toCoord 7 2) 0 }

moveCheckingBoundariesSpec :: Spec
moveCheckingBoundariesSpec = do
  describe "moveCheckingBoundaries" $ do
    it "doesn't achieve anything on an empty player" $
      moveCheckingBoundaries anEmptyPlayer anEmptyPlayer
      `shouldBe`
      (anEmptyPlayer, anEmptyPlayer)
    it "should move missiles on my side to the right" $
      moveCheckingBoundaries aPlayerWithAMissileOnTwoTwo anEmptyPlayer
      `shouldBe`
      (aPlayerWithAMissileOnTwoTwo { missiles0 = addMissile (toCoord 3 2) 0 }, anEmptyPlayer)
    it "should transfer a missile to the other side" $
      moveCheckingBoundaries aPlayerWithAMissileOnSevenTwo anEmptyPlayer
      `shouldBe`
      (aPlayerWithAMissileOnSevenTwo { missiles0          = 0,
                                       missilesOtherSide0 = addMissile (toCoord 7 2) 0 },
       anEmptyPlayer)

updateMoveSpec :: Spec
updateMoveSpec = do
  describe "updateMove" $ do
    it "should do nothing when given the nothing command" $
      (updateMove nothingCommand aPlayer) `shouldBe` aPlayer
    -- TODO: handle deconstruct
    -- it "should deconstruct a tower at the given coordinates when given that command" $
    --   (updateMove (Deconstruct 0 0) aPlayer) `shouldBe` aPlayer { towerMap = M.empty }
    -- it "should deconstruct a tower at the given non-origin coordinates when given that command" $
    --   (updateMove (Deconstruct 6 2) aPlayerWithNonZeroEnergy) `shouldBe` aPlayerWithNonZeroEnergy { towerMap = M.empty }
    it "should build an attack tower at the given coordinates when given that command" $
      updateMove (toEfficientCommand (Build (toCoord 6 2) ATTACK)) aPlayer
      `shouldBe`
      aPlayer { attackTowersUnderConstruction = addBuilding (toCoord 6 2) 0,
                energy                        = -30,
                attackTowersPerRow            = UV.fromList [0, 0, 1, 0, 0, 0, 0, 0] }
    it "should build a defense tower at the given coordinates when given that command" $
      updateMove (toEfficientCommand (Build (toCoord 6 2) DEFENSE)) aPlayer
      `shouldBe`
      aPlayer { defenseTowersUnderConstruction2 = addBuilding (toCoord 6 2) 0,
                energy                          = -30 }
    it "should build a energy tower at the given coordinates when given that command" $
      updateMove (toEfficientCommand (Build (toCoord 6 2) ENERGY)) aPlayer
      `shouldBe`
      aPlayer { energyTowersUnderConstruction = addBuilding (toCoord 6 2) 0,
                energy                        = -20,
                energyGenPerTurn              = 3,
                energyTowersPerRow            = UV.fromList [0, 0, 1, 0, 0, 0, 0, 0] }

-- TODO (!!!) implement the missile movement/collission spec here !!!
