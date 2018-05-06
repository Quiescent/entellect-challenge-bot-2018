{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Interpretor (repl,
                    Player(..),
                    PlayerType(..),
                    Missile(..),
                    Cell(..),
                    BuildingType(..),
                    Building(..),
                    CellStateContainer(..),
                    BuildingPriceIndex(..),
                    GameDetails(..),
                    GameState(..))
  where

import Data.Aeson (FromJSON,
                   parseJSON,
                   withObject,
                   (.:),
                   ToJSON,
                   toJSON,
                   object,
                   (.=))
import Data.Vector as V
import GHC.Generics (Generic)

data PlayerType =
  A | B deriving (Show, Generic)

instance FromJSON PlayerType
instance ToJSON   PlayerType

data Player = Player { playerType :: PlayerType,
                       energy     :: Int,
                       health     :: Int,
                       hitsTaken  :: Int,
                       score      :: Int }
              deriving (Show, Generic)

instance FromJSON Player
instance ToJSON   Player

data Missile = Missile { damage :: Int, speed :: Int }
  deriving (Show, Generic)

instance FromJSON Missile
instance ToJSON   Missile

data Cell = Cell { x :: Int, y :: Int, owner :: PlayerType }
  deriving (Show, Generic)

instance FromJSON Cell
instance ToJSON   Cell

data BuildingType = Defense | Attack | Energy
  deriving (Show, Generic)

instance FromJSON BuildingType
instance ToJSON   BuildingType

data Building = Building { integrity              :: Int,
                           constructionTimeLeft   :: Int,
                           price                  :: Int,
                           weaponDamage           :: Int,
                           weaponSpeed            :: Int,
                           weaponCooldownTimeLeft :: Int,
                           weaponCooldownPeriod   :: Int,
                           destroyScore           :: Int,
                           energyGeneratedPerTurn :: Int,
                           buildingType           :: BuildingType }
                deriving (Show, Generic)

instance FromJSON Building
instance ToJSON   Building

data CellStateContainer = CellStateContainer { xPos      :: Int,
                                               yPos      :: Int,
                                               cellOwner :: PlayerType,
                                               buildings :: [Building],
                                               missiles  :: [Missile] }
                          deriving (Show, Generic)

instance FromJSON CellStateContainer
instance ToJSON   CellStateContainer

data BuildingPriceIndex = BuildingPriceIndex { attackTowerCost  :: Int,
                                               defenseTowerCost :: Int,
                                               energyTowerCost  :: Int }
                          deriving (Show, Generic)

instance FromJSON BuildingPriceIndex where
  parseJSON = withObject "BuildingPriceIndex" $ \ v -> 
    BuildingPriceIndex <$> v .: "ATTACK"
                       <*> v .: "DEFENSE"
                       <*> v .: "ENERGY"
instance ToJSON BuildingPriceIndex where
  toJSON (BuildingPriceIndex attackCost defenseCost energyCost) =
    object ["ATTACK"  .= attackCost,
            "DEFENSE" .= defenseCost,
            "ENERGY"  .= energyCost]

data GameDetails = GameDetails { round          :: Int,
                                 mapWidth       :: Int,
                                 mapHeight      :: Int,
                                 buildingPrices :: BuildingPriceIndex }
                   deriving (Show, Generic)

instance FromJSON GameDetails
instance ToJSON   GameDetails

data GameState = GameState { players     :: [Player],
                             gameMap     :: [CellStateContainer],
                             gameDetails :: GameDetails}

instance FromJSON GameState where
  parseJSON = withObject "GameState" $ \ v -> do
    players       <- v .: "players"
    playersList   <- Prelude.mapM parseJSON $ V.toList players
    gameMapObject <- v .: "gameMap"
    gameMap       <- Prelude.mapM parseJSON $ V.toList gameMapObject
    gameDetails   <- v .: "gameDetails"
    return $ GameState playersList gameMap gameDetails

instance ToJSON GameState where
  toJSON (GameState players gameMap gameDetails) =
    object ["players" .= players, "gameMap" .= gameMap, "gameDetails" .= gameDetails]

stateFilePath :: String
stateFilePath = "state.json"

commandFilePath :: String
commandFilePath = "command.txt"

readGameState :: IO GameState
readGameState = return (GameState [] [] (GameDetails 0 0 0 (BuildingPriceIndex 0 0 0)))

printGameState :: string ->  IO ()
printGameState gameState = return ()

type Command = String

repl :: (GameState -> Command) -> IO ()
repl evaluate = fmap evaluate readGameState >>= printGameState
