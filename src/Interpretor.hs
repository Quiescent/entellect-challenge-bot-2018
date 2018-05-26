{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Interpretor (repl,
                    Player(..),
                    PlayerType(..),
                    Missile(..),
                    CellContents(..),
                    BuildingType(..),
                    Building(..),
                    CellStateContainer(..),
                    BuildingPriceIndex(..),
                    GameDetails(..),
                    GameState(..),
                    Command(..),
                    BuildingStats(..),
                    TowerStats(..),
                    SparseMap,
                    DenseMap)
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
import Data.Map.Strict as M

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

-- Remove the silly x and y disp
data Missile = Missile { damage :: Int,
                         speed  :: Int,
                         owner  :: PlayerType,
                         xDisp  :: Int,
                         yDisp  :: Int }
  deriving (Show, Generic, Eq)

instance FromJSON Missile where
  parseJSON = withObject "Missile" $ \ v ->
    Missile <$> v.: "damage"
            <*> v.: "speed"
            <*> v.: "playerType"
            <*> v.: "x"
            <*> v.: "y"
instance ToJSON Missile where
  toJSON (Missile damage' speed' owner' xDisp' yDisp') =
    object ["damage"     .= damage',
            "speed"      .= speed',
            "playerType" .= owner',
            "x"          .= xDisp',
            "y"          .= yDisp']
    

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

data GameDetails = GameDetails { round             :: Int,
                                 mapWidth          :: Int,
                                 mapHeight         :: Int,
                                 roundIncomeEnergy :: Int,
                                 buildingPrices    :: BuildingPriceIndex,
                                 buildingsStats    :: BuildingStats }
                   deriving (Show, Generic, Eq)

instance FromJSON GameDetails
instance ToJSON   GameDetails

data BuildingStats = BuildingStats { attackTowerStats  :: TowerStats,
                                     defenseTowerStats :: TowerStats,
                                     energyTowerStats  :: TowerStats }
                   deriving (Show, Generic, Eq)

instance FromJSON BuildingStats where
  parseJSON = withObject "BuildingStats" $ \ v ->
    BuildingStats <$> v.: "ATTACK"
                  <*> v.: "DEFENSE"
                  <*> v.: "ENERGY"
instance ToJSON BuildingStats where
  toJSON (BuildingStats attackTowerStats' defenseTowerStats' energyTowerStats') =
    object ["ATTACK"  .= attackTowerStats',
            "DEFENSE" .= defenseTowerStats',
            "ENERGY"  .= energyTowerStats']

data TowerStats = TowerStats { initialIntegrity            :: Int,
                               constructionTime            :: Int,
                               towerPrice                  :: Int,
                               towerWeaponDamage           :: Int,
                               towerWeaponSpeed            :: Int,
                               towerWeaponCooldownPeriod   :: Int,
                               towerEnergyGeneratedPerTurn :: Int,
                               towerDestroyMultiplier      :: Int,
                               towerConstructionScore      :: Int }
                  deriving (Show, Generic, Eq)

instance FromJSON TowerStats where
  parseJSON = withObject "TowerStats" $ \ v ->
    TowerStats <$> v.: "health"
               <*> v.: "constructionTime"
               <*> v.: "price"
               <*> v.: "weaponDamage"
               <*> v.: "weaponSpeed"
               <*> v.: "weaponCooldownPeriod"
               <*> v.: "energyGeneratedPerTurn"
               <*> v.: "destroyMultiplier"
               <*> v.: "constructionScore"
instance ToJSON TowerStats where
  toJSON (TowerStats health'
                     constructionTime'
                     price'
                     weaponDamage'
                     weaponSpeed'
                     weaponCooldownPeriod'
                     energyGeneratedPerTurn'
                     destroyMultiplier'
                     constructionScore') =
    object ["health"                 .= health',
            "constructionTime"       .= constructionTime',
            "price"                  .= price',
            "weaponDamage"           .= weaponDamage',
            "weaponSpeed"            .= weaponSpeed',
            "weaponCooldownPeriod"   .= weaponCooldownPeriod',
            "energyGeneratedPerTurn" .= energyGeneratedPerTurn',
            "destroyMultiplier"      .= destroyMultiplier',
            "constructionScore"      .= constructionScore']

-- TODO change to playerA and playerB
data GameState = GameState { players     :: V.Vector Player,
                             gameMap     :: SparseMap,
                             gameDetails :: GameDetails }
                 deriving (Show, Generic, Eq)

instance FromJSON GameState where
  parseJSON = withObject "GameState" $ \ v -> do
    players'      <- v.: "players"
    denseGameMap  <- v.: "gameMap"
    gameDetails'  <- v.: "gameDetails"
    return $ GameState players'
      (toSparseMap denseGameMap)
      gameDetails'
-- TODO: check whether this works (in particular the sparse to dense
-- map part)
instance ToJSON   GameState where
  toJSON (GameState players' gameMap' gameDetails') =
    object ["players"        .= players',
            "gameMap"        .= toDenseMap gameMap' (mapWidth gameDetails')
                                                    (mapHeight gameDetails'),
            "buildingPrices" .= gameDetails']

data CellContents = CellContents { buildingInCell :: Maybe Building,
                                   missilesInCell :: V.Vector Missile }
          deriving (Show, Generic, Eq)

type SparseMap = M.Map (Int, Int) CellContents

type DenseMap = V.Vector (V.Vector CellStateContainer) 

toSparseMap :: DenseMap -> SparseMap
toSparseMap denseMap =
  V.foldr insertRow M.empty $ V.zip (V.fromList [0..(V.length denseMap)]) denseMap
  where
    rowIndices = (V.fromList [0..(V.length (denseMap V.! 0))])
    insertRow (x, row) sparseMap =
      V.foldr (insertCell x) sparseMap $ V.zip rowIndices row
    insertCell x (y, (CellStateContainer _ _ _ buildings' missiles')) sparseMap =
      let building' = if V.null buildings' then Nothing else Just (buildings' V.! 0)
      in if (V.null buildings' && V.null missiles')
         then sparseMap
         else M.insert (x, y) (CellContents building' missiles') sparseMap

-- TODO Implement
toDenseMap :: SparseMap -> Int -> Int -> DenseMap
toDenseMap sparseMap maxX maxY = V.empty

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
