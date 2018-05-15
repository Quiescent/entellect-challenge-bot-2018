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
                    GameState(..),
                    Command(..))
  where

import Data.Aeson (decode,
                   FromJSON,
                   parseJSON,
                   withObject,
                   (.:),
                   ToJSON,
                   toJSON,
                   object,
                   (.=))
import Data.Vector as V
import GHC.Generics (Generic)
import Data.ByteString.Lazy as B

data PlayerType =
  A | B deriving (Show, Generic, Eq)

instance FromJSON PlayerType
instance ToJSON   PlayerType

data Player = Player { playerType :: PlayerType,
                       energy     :: Int,
                       health     :: Int,
                       hitsTaken  :: Int,
                       score      :: Int }
              deriving (Show, Generic, Eq)

instance FromJSON Player
instance ToJSON   Player

data Missile = Missile { damage :: Int, speed :: Int }
  deriving (Show, Generic, Eq)

instance FromJSON Missile
instance ToJSON   Missile

data Cell = Cell { x :: Int, y :: Int, owner :: PlayerType }
  deriving (Show, Generic, Eq)

instance FromJSON Cell
instance ToJSON   Cell

data BuildingType = DEFENSE | ATTACK | ENERGY
  deriving (Show, Generic, Eq)

instance FromJSON BuildingType
instance ToJSON   BuildingType

data Building = Building { integrity              :: Int,
                           constructionTimeLeft   :: Int,
                           price                  :: Int,
                           weaponDamage           :: Int,
                           weaponSpeed            :: Int,
                           weaponCooldownTimeLeft :: Int,
                           weaponCooldownPeriod   :: Int,
                           destroyMultiplier      :: Int,
                           constructionScore      :: Int,
                           energyGeneratedPerTurn :: Int,
                           buildingType           :: BuildingType,
                           buildingX              :: Int,
                           buildingY              :: Int,
                           buildingOwner          :: PlayerType }
                deriving (Show, Generic, Eq)

instance FromJSON Building where
  parseJSON = withObject "Building" $ \ v -> 
    Building <$> v .: "health"
             <*> v .: "constructionTimeLeft"
             <*> v .: "price"
             <*> v .: "weaponDamage"
             <*> v .: "weaponSpeed"
             <*> v .: "weaponCooldownTimeLeft"
             <*> v .: "weaponCooldownPeriod"
             <*> v .: "destroyMultiplier"
             <*> v .: "constructionScore"
             <*> v .: "energyGeneratedPerTurn"
             <*> v .: "buildingType"
             <*> v .: "x"
             <*> v .: "y"
             <*> v .: "playerType"
instance ToJSON Building where
  toJSON (Building integrity'
                   constructionTimeLeft'
                   price'
                   weaponDamage'
                   weaponSpeed'
                   weaponCooldownTimeLeft'
                   weaponCooldownPeriod'
                   destroyMultiplier'
                   constructionScore'
                   energyGeneratedPerTurn'
                   buildingType'
                   buildingX'
                   buildingY'
                   buildingOwner') =
    object ["health"                 .= integrity',
            "constructionTimeLeft"   .= constructionTimeLeft',
            "price"                  .= price',
            "weaponDamage"           .= weaponDamage',
            "weaponSpeed"            .= weaponSpeed',
            "weaponCooldownTimeLeft" .= weaponCooldownTimeLeft',
            "weaponCooldownPeriod"   .= weaponCooldownPeriod',
            "destroyMultiplier"      .= destroyMultiplier',
            "constructionScore"      .= constructionScore',
            "energyGeneratedPerTurn" .= energyGeneratedPerTurn',
            "buildingType"           .= buildingType',
            "x"                      .= buildingX',
            "y"                      .= buildingY',
            "playerType"             .= buildingOwner']

data CellStateContainer = CellStateContainer { xPos      :: Int,
                                               yPos      :: Int,
                                               cellOwner :: PlayerType,
                                               buildings :: V.Vector Building,
                                               missiles  :: V.Vector Missile }
                          deriving (Show, Generic, Eq)

instance FromJSON CellStateContainer where
  parseJSON = withObject "CellStateContainer" $ \ v -> do
    x'          <- v .: "x"
    y'          <- v .: "y"
    cellOwner'  <- v .: "cellOwner"
    buildings'  <- v .: "buildings"
    missiles'   <- v .: "missiles"
    return $ CellStateContainer x'
                                y'
                                cellOwner'
                                buildings'
                                missiles'

instance ToJSON CellStateContainer where
  toJSON (CellStateContainer xPos'
                             yPos'
                             cellOwner'
                             buildings'
                             missiles') =
    object ["x"         .= xPos',
            "y"         .= yPos',
            "cellOwner" .= cellOwner',
            "buildings" .= buildings',
            "missiles"  .= missiles']

data BuildingPriceIndex = BuildingPriceIndex { attackTowerCost  :: Int,
                                               defenseTowerCost :: Int,
                                               energyTowerCost  :: Int }
                          deriving (Show, Generic, Eq)

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
                   deriving (Show, Generic, Eq)

instance FromJSON GameDetails
instance ToJSON   GameDetails

data GameState = GameState { players     :: V.Vector Player,
                             gameMap     :: V.Vector (V.Vector CellStateContainer),
                             gameDetails :: GameDetails }
                 deriving (Show, Generic, Eq)

instance FromJSON GameState
instance ToJSON   GameState

stateFilePath :: String
stateFilePath = "state.json"

commandFilePath :: String
commandFilePath = "command.txt"

readGameState :: IO GameState
readGameState = do
  stateString <- B.readFile stateFilePath
  let Just state = decode stateString
  return state

data Command = Command { xCoord   :: Int,
                         yCoord   :: Int,
                         building :: BuildingType }
               | NothingCommand

instance Show Command where
  show (Command x' y' building') =
    show x' Prelude.++ "," Prelude.++ show y' Prelude.++ ","  Prelude.++ case building' of
    DEFENSE -> "0"
    ATTACK  -> "1"
    ENERGY  -> "2"
  show NothingCommand = ""

printCommand :: Command ->  IO ()
printCommand = Prelude.writeFile commandFilePath . show

repl :: (GameState -> Command) -> IO ()
repl evaluate = fmap evaluate readGameState >>= printCommand
