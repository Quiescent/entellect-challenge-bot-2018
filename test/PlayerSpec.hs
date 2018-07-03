module PlayerSpec where

import Player
import Interpretor
import GameMap

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
anAttackTower = (Building { integrity              = 5,
                            weaponCooldownTimeLeft = 0,
                            buildingType           = ATTACK })

anEnergyTower :: Building
anEnergyTower = (Building { integrity              = 5,
                            weaponCooldownTimeLeft = 0,
                            buildingType           = ENERGY })

aDefenseTower :: Building
aDefenseTower = (Building { integrity              = 20,
                            weaponCooldownTimeLeft = 0,
                            buildingType           = DEFENSE })

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
                                     hitsTaken         = 3,
                                     score             = 0,
                                     towerMap          = M.fromList [(2, M.fromList [(6, anAttackTower)])],
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
    it "should reset the cooldown of the tower at 0, 0 and create a missile there" $
      (resetCooldownAndCreateMissile aPlayer 0 0 10 20 30)
      `shouldBe`
      (aPlayer { towerMap      = M.fromList [(0, M.fromList [(0, anAttackTower { weaponCooldownTimeLeft = 10 })])],
                 ownedMissiles = [(Missile 20 30 0 0)] })

mapMissilesSpec :: Spec
mapMissilesSpec = do
  describe "mapMissiles" $ do
    it "should modify produce the same player when the map function is `id'" $
      (mapMissiles id aPlayer) `shouldBe` aPlayer

incrementHitsTakenSpec :: Spec
incrementHitsTakenSpec = do
  describe "incrementHitsTaken" $ do
    it "should produce a player with one hit taken when that player has taken no hits" $
      (incrementHitsTaken aPlayer) `shouldBe` (aPlayer { hitsTaken = 1 })
    it "should produce a player with one more hit taken when that player has taken some hits" $
      (incrementHitsTaken aPlayerWithNonZeroEnergy) `shouldBe` (aPlayerWithNonZeroEnergy { hitsTaken = 4 })

aTowerMap :: TowerMap
aTowerMap = M.fromList [(2, M.fromList [(0, anAttackTower)])]

updateTowerMapSpec :: Spec
updateTowerMapSpec = do
  describe "updateEnergy" $ do
    it "should produce a player with an empty tower map when supplied with an empty tower map" $
      (updateTowerMap M.empty aPlayer) `shouldBe` (aPlayer { towerMap = M.empty })
    it "should produce a player with the given tower map when supplied with a tower map" $
      (updateTowerMap aTowerMap aPlayer) `shouldBe` (aPlayer { towerMap = aTowerMap })

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
      (buildingFromStats ATTACK (TowerStats 0 1 2 3 4 5 6 7 8)) `shouldBe` (Building 0 0 ATTACK)

updateMissilesSpec :: Spec
updateMissilesSpec = do
  describe "updateMissiles" $ do
    it "should swap in the given list of missiles" $
      (updateMissiles [(Missile 10 20 0 2)] aPlayer) `shouldBe` (aPlayer { ownedMissiles = [(Missile 10 20 0 2)] })

mapMapSpec :: Spec
mapMapSpec = do
  describe "mapMap" $ do
    it "should do nothing to the map when the given function is `id'" $
      (mapMap id aPlayer) `shouldBe` aPlayer
    it "should remove the tower at 0 0 when given removeAt 0 0" $
      (mapMap (removeAt (0, 0)) aPlayer) `shouldBe` (aPlayer { towerMap = M.empty })

buildSpec :: Spec
buildSpec = do
  describe "build" $ do
    it "should produce a building at the given coordinates in the construction queue" $
      (build 20 4 5 anAttackTower aPlayer) `shouldBe` (aPlayer { constructionQueue = PQ.singleton (20, (4, 5), anAttackTower) })
    it "should do nothing if the target square is occupied" $
      (build 20 0 0 anAttackTower aPlayer) `shouldBe` aPlayer

deconstructAtSpec :: Spec
deconstructAtSpec = do
  describe "deconstructAt" $ do
    it "should remove a tower at the given coordinates" $
      (deconstructAt 0 0 aPlayer) `shouldBe` (aPlayer { towerMap = M.empty })
    it "should remove a tower at the given non-origin coordinates" $
      (deconstructAt 6 2 aPlayerWithNonZeroEnergy) `shouldBe` (aPlayerWithNonZeroEnergy { towerMap = M.empty })

genericDetails :: GameDetails
genericDetails = (GameDetails { roundIncomeEnergy = 5,
                                buildingPrices    = (BuildingPriceIndex { attackTowerCost  = 30,
                                                                          defenseTowerCost = 30,
                                                                          energyTowerCost  = 20,
                                                                          teslaTowerCost   = 300 }),
                                buildingsStats = (BuildingStats { attackTowerStats  =
                                                                  (TowerStats { initialIntegrity       = 5,
                                                                                constructionTime       = 2,
                                                                                towerPrice             = 30,
                                                                                weaponDamage           = 5,
                                                                                weaponSpeed            = 1,
                                                                                weaponCooldownPeriod   = 3,
                                                                                energyGeneratedPerTurn = 0,
                                                                                destroyMultiplier      = 1,
                                                                                constructionScore      = 1 }),
                                                                  defenseTowerStats =
                                                                  (TowerStats { initialIntegrity       = 20,
                                                                                constructionTime       = 4,
                                                                                towerPrice             = 30,
                                                                                weaponDamage           = 0,
                                                                                weaponSpeed            = 0,
                                                                                weaponCooldownPeriod   = 0,
                                                                                energyGeneratedPerTurn = 0,
                                                                                destroyMultiplier      = 1,
                                                                                constructionScore      = 1 }),
                                                                  energyTowerStats  =
                                                                  (TowerStats { initialIntegrity       = 5,
                                                                                constructionTime       = 2,
                                                                                towerPrice             = 20,
                                                                                weaponDamage           = 0,
                                                                                weaponSpeed            = 0,
                                                                                weaponCooldownPeriod   = 0,
                                                                                energyGeneratedPerTurn = 3,
                                                                                destroyMultiplier      = 1,
                                                                                constructionScore      = 1 }),
                                                                  teslaTowerStats   =
                                                                  (TowerStats { initialIntegrity       = 5,
                                                                                constructionTime       = 11,
                                                                                towerPrice             = 300,
                                                                                weaponDamage           = 20,
                                                                                weaponSpeed            = 0,
                                                                                weaponCooldownPeriod   = 10,
                                                                                energyGeneratedPerTurn = 0,
                                                                                destroyMultiplier      = 1,
                                                                                constructionScore      = 1 })}) })

updateMoveSpec :: Spec
updateMoveSpec = do
  describe "updateMove" $ do
    it "should do nothing when given the nothing command" $
      (updateMove genericDetails NothingCommand aPlayer) `shouldBe` aPlayer
    it "should deconstruct a tower at the given coordinates when given that command" $
      (updateMove genericDetails (Deconstruct 0 0) aPlayer) `shouldBe` aPlayer { towerMap = M.empty }
    it "should deconstruct a tower at the given non-origin coordinates when given that command" $
      (updateMove genericDetails (Deconstruct 6 2) aPlayerWithNonZeroEnergy) `shouldBe` aPlayerWithNonZeroEnergy { towerMap = M.empty }
    it "should build an attack tower at the given coordinates when given that command" $
      updateMove genericDetails (Build 6 2 ATTACK) aPlayer `shouldBe` aPlayer { constructionQueue = PQ.singleton (2, (6, 2), anAttackTower) }
    it "should build a defense tower at the given coordinates when given that command" $
      updateMove genericDetails (Build 6 2 DEFENSE) aPlayer `shouldBe` aPlayer { constructionQueue = PQ.singleton (4, (6, 2), aDefenseTower) }
    it "should build a energy tower at the given coordinates when given that command" $
      updateMove genericDetails (Build 6 2 ENERGY) aPlayer `shouldBe` aPlayer { constructionQueue = PQ.singleton (2, (6, 2), anEnergyTower) }
