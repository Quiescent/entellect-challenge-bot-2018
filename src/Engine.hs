module Engine (tickEngine, energyGenPerTurn)
  where

import Interpretor (GameState(..), Player(..))

import Player
import Building
import GameState
import Magic
import BitSetMap

tickEngine :: GameState -> GameState
tickEngine = resetIronCurtain . incrementRound . gainEnergy . collideMissiles . tickBuildings

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
