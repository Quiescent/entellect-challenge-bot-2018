{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Interpretor (repl,
                    parseStateString,
                    Player(..),
                    Missile(..),
                    BuildingType(..),
                    Building(..),
                    Command(..),
                    GameState(..),
                    TowerMap,
                    BuildingUnderConstruction,
                    ConstructionQueue)
  where

import Data.Aeson (decode,
                   FromJSON,
                   parseJSON,
                   withObject,
                   (.:))
import qualified Data.PQueue.Min      as PQ
import qualified Data.Vector          as V
import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap          as M
import qualified Data.List            as L
import GHC.Generics (Generic(..))
import Control.DeepSeq

import Coord

data PlayerType =
  A | B deriving (Show, Generic, Eq)

instance FromJSON PlayerType

data Missile = Missile { xDisp  :: Int,
                         yDisp  :: Int }
  deriving (Show, Generic, Eq, Ord)

instance NFData Missile

data ScratchMissile = ScratchMissile Int
                                     Int
                                     PlayerType
                                     Int
                                     Int
  deriving (Show, Generic, Eq)

instance FromJSON ScratchMissile where
  parseJSON = withObject "Missile" $ \ v ->
    ScratchMissile <$> v.: "damage"
                   <*> v.: "speed"
                   <*> v.: "playerType"
                   <*> v.: "x"
                   <*> v.: "y"

data BuildingType = DEFENSE | ATTACK | ENERGY | TESLA
  deriving (Show, Generic, Eq, Ord)

instance NFData BuildingType
instance FromJSON BuildingType

data Building = Building { integrity              :: Int,
                           weaponCooldownTimeLeft :: Int,
                           buildingType           :: BuildingType }
              deriving (Show, Eq, Generic, Ord)

instance NFData Building

data ScratchBuilding = ScratchBuilding Int
                                       Int
                                       Int
                                       BuildingType
                                       PlayerType
                deriving (Show, Generic, Eq)

instance FromJSON ScratchBuilding where
  parseJSON = withObject "Building" $ \ v -> 
    ScratchBuilding <$> v .: "health"
                    <*> v .: "constructionTimeLeft"
                    <*> v .: "weaponCooldownTimeLeft"
                    <*> v .: "buildingType"
                    <*> v .: "playerType"

-- TODO consider making this data to showup errors in how I've done this
type BuildingUnderConstruction = (Int, Coord, Building)

instance {-# OVERLAPPING #-} Ord BuildingUnderConstruction where 
  (<) (x, _, _) (y, _, _) = x < y
  (<=) (x, _, _) (y, _, _) = x < y
  (>) (x, _, _) (y, _, _) = x > y
  (>=) (x, _, _) (y, _, _) = x >= y
  min a@(x, _, _) b@(y, _, _) = if x < y then a else b
  max a@(x, _, _) b@(y, _, _) = if x > y then a else b

type ConstructionQueue = PQ.MinQueue BuildingUnderConstruction

data Player = Player { energy            :: Int,
                       health            :: Int,
                       hitsTaken         :: Int,
                       energyGenPerTurn  :: Float,
                       attackPerRow      :: M.IntMap Int,
                       defensePerRow     :: M.IntMap Int,
                       towerMap          :: TowerMap,
                       constructionQueue :: ConstructionQueue,
                       ownedMissiles     :: [Missile] }
              deriving (Show, Generic)

-- Allows for built in sorting
toOrderedBuildingUnderConstruction :: BuildingUnderConstruction -> (Int, Int, Coord, Building)
toOrderedBuildingUnderConstruction (x, y, z) = (0, x, y, z)

compareFullyOrderedConstruction :: BuildingUnderConstruction -> BuildingUnderConstruction -> Ordering
compareFullyOrderedConstruction x y = compare (toOrderedBuildingUnderConstruction x) (toOrderedBuildingUnderConstruction y)

instance Eq Player where
  (==) (Player energyA healthA hitsTakenA energyGenPerTurnA attackPerRowA defensePerRowA towerMapA constructionQueueA ownedMissilesA)
       (Player energyB healthB hitsTakenB energyGenPerTurnB attackPerRowB defensePerRowB towerMapB constructionQueueB ownedMissilesB)
    = energyA                                 == energyB &&
      healthA                                 == healthB &&
      hitsTakenA                              == hitsTakenB &&
      energyGenPerTurnA                       == energyGenPerTurnB &&
      attackPerRowA                           == attackPerRowB &&
      defensePerRowA                          == defensePerRowB &&
      towerMapA                               == towerMapB &&
      L.sort ownedMissilesA                   == L.sort ownedMissilesB &&
      (L.sortBy compareFullyOrderedConstruction $ PQ.toList constructionQueueA) == (L.sortBy compareFullyOrderedConstruction $ PQ.toList constructionQueueB)

instance NFData Player

data ScratchPlayer = ScratchPlayer PlayerType Int Int Int
                   deriving (Show, Generic, Eq)

instance FromJSON ScratchPlayer where
  parseJSON = withObject "ScratchPlayer" $ \ v -> do
    playerType' <- v .: "playerType"
    energy''    <- v .: "energy"
    health''    <- v .: "health"
    hitsTaken'' <- v .: "hitsTaken"
    return $ ScratchPlayer playerType'
                           energy''
                           health''
                           hitsTaken''

data GameState = GameState { me       :: Player,
                             oponent  :: Player }
  deriving (Show, Eq, Generic)

instance NFData GameState

instance FromJSON GameState where
  parseJSON = withObject "GameState" $ \ v -> do
    players'      <- v .: "players"
    denseGameMap  <- v .: "gameMap"
    let (GameState me' oponent') = convertDenseMap denseGameMap
    let (((ScratchPlayer _
                         aEnergy
                         aHealth
                         aHitsTaken),
           (ScratchPlayer _
                          bEnergy
                          bHealth
                          bHitsTaken))) = extractPlayers players'
    return (GameState me' { energy    = aEnergy,
                            health    = aHealth,
                            hitsTaken = aHitsTaken }
                      oponent' { energy    = bEnergy,
                                 health    = bHealth,
                                 hitsTaken = bHitsTaken })

extractPlayers :: V.Vector ScratchPlayer -> (ScratchPlayer, ScratchPlayer)
extractPlayers players =
  let firstPlayer@(ScratchPlayer firstPlayerType _ _ _)  = players V.! 0
      secondPlayer = players V.! 1
  in if firstPlayerType == A
     then (firstPlayer,  secondPlayer)
     else (secondPlayer, firstPlayer)

type TowerMap = M.IntMap Building

data CellStateContainer = CellStateContainer Int
                                             Int
                                             PlayerType
                                             (V.Vector ScratchBuilding)
                                             (V.Vector ScratchMissile)
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

type DenseMap = V.Vector DenseRow

type DenseRow = V.Vector CellStateContainer

emptyPlayer :: Player
emptyPlayer = Player 0 0 0 0 M.empty M.empty M.empty PQ.empty []

emptyGameState :: GameState
emptyGameState = GameState emptyPlayer emptyPlayer

convertDenseMap :: DenseMap -> GameState
convertDenseMap = V.foldr accRow emptyGameState

accRow :: DenseRow -> GameState -> GameState
accRow = flip (V.foldr accCell)

accCell :: CellStateContainer -> GameState -> GameState
accCell (CellStateContainer x' y' _ buildings' missiles') =
  accMissiles missiles' . accBuildings x' y' buildings'

accMissiles :: V.Vector ScratchMissile -> GameState -> GameState
accMissiles missiles' gameState@(GameState me' oponent') =
  gameState { me      = me'      { ownedMissiles = myExistingMissiles       ++ myMissiles },
              oponent = oponent' { ownedMissiles = oponentsExistingMissiles ++ oponentsMissiles } }
  where
    myExistingMissiles             = ownedMissiles me'
    oponentsExistingMissiles       = ownedMissiles oponent'
    (myMissiles, oponentsMissiles) = (splitMissiles missiles')

splitMissiles :: V.Vector ScratchMissile -> ([Missile], [Missile])
splitMissiles = V.foldr splitMissilesAcc ([], [])

splitMissilesAcc :: ScratchMissile -> ([Missile], [Missile]) -> ([Missile], [Missile])
splitMissilesAcc (ScratchMissile _ _ owner' x' y') (myMissiles, oponentsMissiles) =
  let missile = (Missile x' y')
  in if owner' == A
     then (missile : myMissiles, oponentsMissiles)
     else (myMissiles,           missile : oponentsMissiles)

accBuildings :: Int -> Int -> V.Vector ScratchBuilding -> GameState -> GameState
accBuildings x' y' buildings' =
  if not $ V.null buildings'
  then accBuilding x' y' (buildings' V.! 0)
  else id

accBuilding :: Int -> Int -> ScratchBuilding -> GameState -> GameState
accBuilding x' y' building'@(ScratchBuilding _ _ _ _ A) state =
  state { me = accBuildingToPlayer x' y' building' (me state) }
accBuilding x' y' building'@(ScratchBuilding _ _ _ _ B) state =
  state { oponent = accBuildingToPlayer x' y' building' (oponent state) }

accBuildingToPlayer :: Int -> Int -> ScratchBuilding -> Player -> Player
accBuildingToPlayer x' y' (ScratchBuilding int ctl wctl bt _) player@(Player { towerMap          = towerMap',
                                                                               constructionQueue = constructionQueue' }) =
  let building' = (Building int wctl bt)
  in if ctl < 0
     then player { towerMap = M.insert (toCoord x' y') building' towerMap' }
     else player { constructionQueue = PQ.insert (ctl, toCoord x' y', building') constructionQueue' }

stateFilePath :: String
stateFilePath = "state.json"

commandFilePath :: String
commandFilePath = "command.txt"

parseStateString :: B.ByteString -> GameState
parseStateString stateString =
  let Just state = decode stateString
  in state

readGameState :: IO GameState
readGameState = fmap parseStateString $ B.readFile stateFilePath

data Command = Build { xCoord   :: Int,
                       yCoord   :: Int,
                       building :: BuildingType }
               | Deconstruct { xCoord :: Int,
                               yCoord :: Int }
               | NothingCommand
             deriving (Eq, Generic)

instance NFData Command

instance Show Command where
  show (Build x' y' building') =
    show x' ++ "," ++ show y' ++ ","  ++ case building' of
    DEFENSE -> "0"
    ATTACK  -> "1"
    ENERGY  -> "2"
    TESLA   -> "4"
  show (Deconstruct x' y') =
    show x' ++ "," ++ show y' ++ ","  ++ "3"
  show NothingCommand = ""

printCommand :: Command ->  IO ()
printCommand = writeFile commandFilePath . show

repl :: (GameState -> IO Command) -> IO ()
repl evaluate = readGameState >>= evaluate >>= printCommand
