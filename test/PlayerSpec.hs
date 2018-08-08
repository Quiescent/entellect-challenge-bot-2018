module PlayerSpec where

import Player
import Interpretor
import GameMap
import Buildings
import Coord
import EfficientCommand
import Magic

import qualified Data.IntMap         as M
import qualified Data.PQueue.Min     as PQ
import qualified Data.Vector.Unboxed as UV

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
  mapMissilesSpec                   >>
  updateTowerMapSpec                >>
  takeDamageSpec                    >>
  buildingFromStatsSpec             >>
  updateMissilesSpec                >>
  mapMapSpec                        >>
  buildSpec                         >>
  updateMoveSpec                    >>
  deconstructAtSpec                 >>
  decrementCooldownSpec

aPlayer :: Player
aPlayer = (Player { energy             = 0,
                    health             = 25,
                    energyGenPerTurn   = 0,
                    energyTowersPerRow = UV.fromList (replicate height 0),
                    attackTowersPerRow = UV.fromList (replicate height 0),
                    towerMap           = M.fromList [((toCoord 0 0), attack2)],
                    constructionQueue  = PQ.empty,
                    ownedMissiles      = UV.empty })

aPlayerWithNonZeroEnergy :: Player
aPlayerWithNonZeroEnergy = (Player { energy             = 10,
                                     health             = 15,
                                     energyGenPerTurn   = 0,
                                     energyTowersPerRow = UV.fromList (replicate height 0),
                                     attackTowersPerRow = UV.fromList (replicate height 0),
                                     towerMap           = M.fromList [((toCoord 6 2), attack2)],
                                     constructionQueue  = PQ.empty,
                                     ownedMissiles      = UV.empty })

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

-- Need one for me and one for oponent
-- resetCooldownAndCreateMissileSpec :: Spec
-- resetCooldownAndCreateMissileSpec = do
--   describe "resetCooldown" $ do
--     it "should reset the cooldown of the tower at 0, 0 and create a missile there" $
--       (resetCooldownAndCreateMissile aPlayer 0 0 10)
--       `shouldBe`
--       (aPlayer { towerMap      = M.fromList [((toCoord 0 0), attack3)],
--                  ownedMissiles = UV.fromList [(toCoord 0 0)] })

mapMissilesSpec :: Spec
mapMissilesSpec = do
  describe "mapMissiles" $ do
    it "should modify produce the same player when the map function is `id'" $
      (mapMissiles id aPlayer) `shouldBe` aPlayer

aTowerMap :: TowerMap
aTowerMap = M.fromList [((toCoord 0 2), attack2)]

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
      (buildingFromStats ATTACK) `shouldBe` attack0

updateMissilesSpec :: Spec
updateMissilesSpec = do
  describe "updateMissiles" $ do
    it "should swap in the given list of missiles" $
      (updateMissiles (UV.fromList [(toCoord 0 2)]) aPlayer) `shouldBe` (aPlayer { ownedMissiles = (UV.fromList [(toCoord 0 2)]) })

mapMapSpec :: Spec
mapMapSpec = do
  describe "mapMap" $ do
    it "should do nothing to the map when the given function is `id'" $
      (mapMap id aPlayer) `shouldBe` aPlayer
    it "should remove the tower at 0 0 when given removeAt 0 0" $
      (mapMap (removeAt (toCoord 0 0)) aPlayer) `shouldBe` (aPlayer { towerMap = M.empty })

buildSpec :: Spec
buildSpec = do
  describe "build" $ do
    it "should produce a building at the given coordinates packed into an integer" $
      (build (toCoord 4 5) attack2) `shouldBe` 678
    it "should produce a building at the given coordinate with that coordinate being zero" $
      (build (toCoord 0 0) attack2) `shouldBe` 6

deconstructAtSpec :: Spec
deconstructAtSpec = do
  describe "deconstructAt" $ do
    it "should remove a tower at the given coordinates" $
      (deconstructAt (toCoord 0 0) aPlayer) `shouldBe` (aPlayer { towerMap = M.empty })
    it "should remove a tower at the given non-origin coordinates" $
      (deconstructAt (toCoord 6 2) aPlayerWithNonZeroEnergy) `shouldBe` (aPlayerWithNonZeroEnergy { towerMap = M.empty })

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
      aPlayer { constructionQueue  = PQ.singleton (0, (toCoord 6 2), attack0 ),
                energy             = -30,
                attackTowersPerRow = UV.fromList [0, 0, 1, 0, 0, 0, 0, 0] }
    it "should build a defense tower at the given coordinates when given that command" $
      updateMove (toEfficientCommand (Build (toCoord 6 2) DEFENSE)) aPlayer
      `shouldBe`
      aPlayer { constructionQueue = PQ.singleton (2, (toCoord 6 2), defense4),
                energy            = -30 }
    it "should build a energy tower at the given coordinates when given that command" $
      updateMove (toEfficientCommand (Build (toCoord 6 2) ENERGY)) aPlayer
      `shouldBe`
      aPlayer { constructionQueue  = PQ.singleton (0, (toCoord 6 2), energyTower),
                energy             = -20,
                energyGenPerTurn   = 3,
                energyTowersPerRow = UV.fromList [0, 0, 1, 0, 0, 0, 0, 0] }

decrementCooldownSpec :: Spec
decrementCooldownSpec = do
  describe "decrementCooldown" $ do
    it "should decrement the cooldown of the building at the given coordinates" $
      decrementCooldown (toCoord 0 0) aPlayer
      `shouldBe`
      aPlayer { towerMap = M.fromList [((toCoord 0 0), attack1)] }
