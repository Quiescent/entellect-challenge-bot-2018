{-# LANGUAGE OverloadedStrings #-}

module InterpretorSpec (spec) where

import Interpretor

import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap          as M
import qualified Data.PQueue.Min      as PQ

import Test.Hspec

spec :: Spec
spec = do
    describe "parseStateString" $ do
        it "should parse a state string with an empty map correctly" $
            parseStateString simpleStateString `shouldBe` expectedSimpleStateString

simpleStateString :: B.ByteString
simpleStateString = "{\"gameDetails\": {\"round\": 11,\"maxRounds\": 400,\"mapWidth\": 16,\"mapHeight\": 8,\"roundIncomeEnergy\": 5,\"buildingPrices\": {\"TESLA\": 300,\"DEFENSE\": 30,\"ATTACK\": 30,\"ENERGY\": 20},\"buildingsStats\": {\"TESLA\": {\"health\": 5,\"constructionTime\": 11,\"price\": 300,\"weaponDamage\": 20,\"weaponSpeed\": 0,\"weaponCooldownPeriod\": 10,\"energyGeneratedPerTurn\": 0,\"destroyMultiplier\": 1,\"constructionScore\": 1},\"DEFENSE\": {\"health\": 20,\"constructionTime\": 4,\"price\": 30,\"weaponDamage\": 0,\"weaponSpeed\": 0,\"weaponCooldownPeriod\": 0,\"energyGeneratedPerTurn\": 0,\"destroyMultiplier\": 1,\"constructionScore\": 1},\"ATTACK\": {\"health\": 5,\"constructionTime\": 2,\"price\": 30,\"weaponDamage\": 5,\"weaponSpeed\": 1,\"weaponCooldownPeriod\": 3,\"energyGeneratedPerTurn\": 0,\"destroyMultiplier\": 1,\"constructionScore\": 1},\"ENERGY\": {\"health\": 5,\"constructionTime\": 2,\"price\": 20,\"weaponDamage\": 0,\"weaponSpeed\": 0,\"weaponCooldownPeriod\": 0,\"energyGeneratedPerTurn\": 3,\"destroyMultiplier\": 1,\"constructionScore\": 1}}},\"players\": [{\"playerType\": \"A\",\"energy\": 37,\"health\": 30,\"hitsTaken\": 14,\"score\": 451},{\"playerType\": \"B\",\"energy\": 53,\"health\": 100,\"hitsTaken\": 0,\"score\": 2093}],\"gameMap\": []}"

expectedSimpleStateString :: GameStateContainer
expectedSimpleStateString =
  GameStateContainer (GameState { me      = (Player { energy            = 37,
                                                      health            = 30,
                                                      hitsTaken         = 14,
                                                      score             = 451,
                                                      towerMap          = M.empty,
                                                      constructionQueue = PQ.empty,
                                                      ownedMissiles     = [] }),
                                   oponent = (Player { energy            = 53,
                                                       health            = 100,
                                                       hitsTaken         = 0,
                                                       score             = 451,
                                                       towerMap          = M.empty,
                                                       constructionQueue = PQ.empty,
                                                       ownedMissiles     = [] })})
                     (GameDetails { roundIncomeEnergy = 5,
                                    buildingPrices    = (BuildingPriceIndex { attackTowerCost = 30,
                                                                              defenseTowerCost = 30,
                                                                              teslaTowerCost = 300,
                                                                              energyTowerCost = 20 }),
                                    buildingsStats    = (BuildingStats { attackTowerStats  =
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
