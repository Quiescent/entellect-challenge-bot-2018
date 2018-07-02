module PlayerSpec where

import Player
import Interpretor

import qualified Data.IntMap          as M
import qualified Data.PQueue.Min      as PQ

import Test.Hspec

spec :: Spec
spec =
  updateEnergySpec                  >>
  myPlayerSpec                      >>
  oponentsPlayerSpec                >>
  myEnergySpec                      >>
  oponentsEnergySpec                >>
  myHealthSpec                      >>
  oponentsHealthSpec                >>
  resetCooldownAndCreateMissileSpec >>
  mapMissilesSpec                   >>
  incrementHitsTakenSpec            >>
  updateTowerMapSpec                >>
  takeDamageSpec                    >>
  buildingFromStatsSpec             >>
  updateMissilesSpec                >>
  mapMapSpec                        >>
  buildSpec                         >>
  updateMoveSpec                    >>
  deconstructAtSpec

anAttackTower :: Building
anAttackTower = (Building { integrity              = 10,
                            weaponCooldownTimeLeft = 0,
                            buildingType           = ATTACK })

aPlayer :: Player
aPlayer = (Player { energy            = 0,
                    health            = 25,
                    hitsTaken         = 0,
                    score             = 0,
                    towerMap          = M.fromList [(0, M.fromList [(0, anAttackTower)])],
                    constructionQueue = PQ.empty,
                    ownedMissiles     = [] })

aPlayerWithNonZeroEnergy :: Player
aPlayerWithNonZeroEnergy = (Player { energy            = 10,
                                     health            = 15,
                                     hitsTaken         = 0,
                                     score             = 0,
                                     towerMap          = M.empty,
                                     constructionQueue = PQ.empty,
                                     ownedMissiles     = [] })

aGameState :: GameState
aGameState = (GameState { me      = aPlayer,
                          oponent = aPlayerWithNonZeroEnergy })

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

resetCooldownAndCreateMissileSpec :: Spec
resetCooldownAndCreateMissileSpec = do
  describe "resetCooldown" $ do
    it "TODO: Implement a test!" $
      (resetCooldownAndCreateMissile aPlayer 0 0 10 20 30)
      `shouldBe`
      (aPlayer { towerMap      = M.fromList [(0, M.fromList [(0, anAttackTower { weaponCooldownTimeLeft = 10 })])],
                 ownedMissiles = [(Missile 20 30 0 0)] })

mapMissilesSpec :: Spec
mapMissilesSpec = do
  describe "mapMissiles" $ do
    it "TODO: Implement a test!" $
      0 `shouldBe` 0

incrementHitsTakenSpec :: Spec
incrementHitsTakenSpec = do
  describe "incrementHits" $ do
    it "TODO: Implement a test!" $
      0 `shouldBe` 0

updateTowerMapSpec :: Spec
updateTowerMapSpec = do
  describe "updateEnergy" $ do
    it "TODO: Implement a test!" $
      0 `shouldBe` 0

takeDamageSpec :: Spec
takeDamageSpec = do
  describe "takeDamage" $ do
    it "TODO: Implement a test!" $
      0 `shouldBe` 0

buildingFromStatsSpec :: Spec
buildingFromStatsSpec = do
  describe "buildingFrom" $ do
    it "TODO: Implement a test!" $
      0 `shouldBe` 0

updateMissilesSpec :: Spec
updateMissilesSpec = do
  describe "updateEnergy" $ do
    it "TODO: Implement a test!" $
      0 `shouldBe` 0

mapMapSpec :: Spec
mapMapSpec = do
  describe "mapMap" $ do
    it "TODO: Implement a test!" $
      0 `shouldBe` 0

buildSpec :: Spec
buildSpec = do
  describe "buildSpec" $ do
    it "TODO: Implement a test!" $
      0 `shouldBe` 0

updateMoveSpec :: Spec
updateMoveSpec = do
  describe "updateEnergy" $ do
    it "TODO: Implement a test!" $
      0 `shouldBe` 0

deconstructAtSpec :: Spec
deconstructAtSpec = do
  describe "deconstructAt" $ do
    it "TODO: Implement a test!" $
      0 `shouldBe` 0
