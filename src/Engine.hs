module Engine (tickEngine, energyGenPerTurn)
  where

import Interpretor (GameState(..), Player(..))

import Player
import Building
import GameState
import Magic
import BitSetMap

tickEngine :: GameState -> GameState
tickEngine = gainEnergy . collideMissiles . tickBuildings . tickIronCurtain . incrementRound

tickIronCurtain :: GameState -> GameState
tickIronCurtain gameState = resetIronCurtain $ updateIronCurtainActiveTime gameState

updateIronCurtainActiveTime :: GameState -> GameState
updateIronCurtainActiveTime
  gameState@(GameState { gameRound = gameRound',
                         me        = me'@(Player { activeIronCurtainLifetime =
                                                   myActiveIronCurtainLifetime }),
                         oponent   = oponent'@(Player { activeIronCurtainLifetime =
                                                        oponentsActiveIronCurtainLifetime })}) =
  gameState { me      = me'      { activeIronCurtainLifetime = myNewActiveTime },
              oponent = oponent' { activeIronCurtainLifetime = oponentsNewActiveTime } }
  where
    timeToReset           = mod (gameRound' - 1) 30 == 0 && gameRound' > 1
    myNewActiveTime       = if timeToReset then -1 else myActiveIronCurtainLifetime - 1
    oponentsNewActiveTime = if timeToReset then -1 else oponentsActiveIronCurtainLifetime - 1

resetIronCurtain :: GameState -> GameState
resetIronCurtain gameState@(GameState gameRound' me' oponent') =
  if mod (gameRound' - 1) 30 == 0 && gameRound' > 1
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
