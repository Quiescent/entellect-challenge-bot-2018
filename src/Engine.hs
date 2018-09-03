module Engine (tickEngine, energyGenPerTurn)
  where

import Interpretor (GameState(..), Player(..))

import Player
import Building
import GameState
import Magic
import BitSetMap

tickEngine :: GameState -> GameState
tickEngine = tickIronCurtain . incrementRound . gainEnergy . collideMissiles . tickBuildings

tickIronCurtain :: GameState -> GameState
tickIronCurtain gameState@(GameState { me      = me'@(Player { activeIronCurtainLifetime = myActiveIronCurtainLifetime }),
                                       oponent = oponent'@(Player { activeIronCurtainLifetime = oponentsActiveIronCurtainLifetime })}) =
  resetIronCurtain $
  gameState { me      = me'      { activeIronCurtainLifetime = myActiveIronCurtainLifetime - 1,
                                   isIronCurtainActive       = myRemainingTime > 0 },
              oponent = oponent' { activeIronCurtainLifetime = oponentsActiveIronCurtainLifetime - 1,
                                   isIronCurtainActive       = oponentsRemainingTime > 0}}
  where
    myRemainingTime       = myActiveIronCurtainLifetime - 1
    oponentsRemainingTime = oponentsActiveIronCurtainLifetime - 1

resetIronCurtain :: GameState -> GameState
resetIronCurtain gameState@(GameState gameRound' me' oponent') =
  if mod gameRound' 30 == 0
  then (gameState { me      = me'      { ironCurtainAvailable = True },
                    oponent = oponent' { ironCurtainAvailable = True }})
  else gameState

incrementRound :: GameState -> GameState
incrementRound gameState@(GameState { gameRound = gameRound' }) =
  gameState { gameRound = gameRound' + 1 }

gainEnergy :: GameState -> GameState
gainEnergy = mapMyPlayer (incrementEnergy) . mapOponentsPlayer (incrementEnergy)

incrementEnergy :: Player -> Player
incrementEnergy player' = updateEnergy (energyPerTurn + (energyGenPerTurn player')) player'

energyGenPerTurn :: Player -> Int
energyGenPerTurn =
  (* energyTowerEnergyGeneratedPerTurn) .
  countBuildings .
  energyTowers

collideMissiles :: GameState -> GameState
collideMissiles = collideAndMoveMissiles
