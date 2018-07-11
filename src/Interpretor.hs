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
  (==) (Player energyA healthA hitsTakenA towerMapA constructionQueueA ownedMissilesA)
       (Player energyB healthB hitsTakenB towerMapB constructionQueueB ownedMissilesB)
    = energyA                                 == energyB &&
      healthA                                 == healthB &&
      hitsTakenA                              == hitsTakenB &&
      towerMapA                               == towerMapB &&
      L.sort ownedMissilesA                   == L.sort ownedMissilesB &&
      (L.sortBy compareFullyOrderedConstruction $ PQ.toList constructionQueueA) == (L.sortBy compareFullyOrderedConstruction $ PQ.toList constructionQueueB)

instance NFData Player

data ScratchPlayer = ScratchPlayer { playerType :: PlayerType,
                                     energy'    :: Int,
                                     health'    :: Int,
                                     hitsTaken' :: Int }
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
    let (myTowerMap,
         oponentsTowerMap,
         myQueue,
         oponentsQueue,
         myMissiles,
         oponentsMissiles) = convertDenseMap denseGameMap
    let (((ScratchPlayer _
                         aEnergy
                         aHealth
                         aHitsTaken),
           (ScratchPlayer _
                          bEnergy
                          bHealth
                          bHitsTaken))) = extractPlayers players'
    return (GameState (Player aEnergy
                               aHealth
                               aHitsTaken
                               myTowerMap
                               myQueue
                               myMissiles)
                       (Player bEnergy
                               bHealth
                               bHitsTaken
                               oponentsTowerMap
                               oponentsQueue
                               oponentsMissiles))

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

type RowAccumulator = (TowerMap, TowerMap, ConstructionQueue, ConstructionQueue, [Missile], [Missile])

makeRow :: TowerMap -> TowerMap -> DenseRow -> RowAccumulator
makeRow myTowerMap oponentsTowerMap =
  V.foldr accCell (myTowerMap, oponentsTowerMap, PQ.empty, PQ.empty, [], [])

accCell :: CellStateContainer -> RowAccumulator -> RowAccumulator
accCell (CellStateContainer x' y' _ buildings' missiles') acc@(_, _, _, _, myMissiles, oponentsMissiles) =
  (myTowerMap', oponentsTowerMap', myQueue', oponentsQueue', myMissiles ++ myMissiles', oponentsMissiles ++ oponentsMissiles')
  where
    (myMissiles', oponentsMissiles') = splitMissiles missiles'
    (myTowerMap', oponentsTowerMap', myQueue', oponentsQueue', _, _) =
      if not $ V.null buildings'
      then accBuilding x' y' (buildings' V.! 0) acc
      else acc

accBuilding :: Int -> Int -> ScratchBuilding -> RowAccumulator -> RowAccumulator
accBuilding x' y' (ScratchBuilding int ctl wctl bt A) (myTowerMap, b, queue, d, e, f) =
  let building' = (Building int wctl bt)
  in if ctl < 0
     then (M.insert (toCoord x' y') building' myTowerMap, b, queue,                                           d, e, f)
     else (myTowerMap,                                    b, PQ.insert (ctl, toCoord x' y', building') queue, d, e, f)
accBuilding x' y' (ScratchBuilding int ctl wctl bt B) (a, oponentsTowerMap, c, queue, e, f) =
  let building' = (Building int wctl bt)
  in if ctl < 0
     then (a, M.insert (toCoord x' y') building' oponentsTowerMap, c, queue,                                           e, f)
     else (a, oponentsTowerMap,                                    c, PQ.insert (ctl, toCoord x' y', building') queue, e, f)

splitMissiles :: V.Vector ScratchMissile -> ([Missile], [Missile])
splitMissiles =
  V.foldr splitMissilesAcc ([], [])

splitMissilesAcc :: ScratchMissile -> ([Missile], [Missile]) -> ([Missile], [Missile])
splitMissilesAcc (ScratchMissile _ _ owner' x' y') (myMissiles, oponentsMissiles) =
  let missile = (Missile x' y')
  in if owner' == A
     then (missile : myMissiles, oponentsMissiles)
     else (myMissiles,           missile : oponentsMissiles)

type MapAccumulator = (TowerMap, TowerMap, ConstructionQueue, ConstructionQueue, [Missile], [Missile])

convertDenseMap :: DenseMap -> MapAccumulator
convertDenseMap denseMap =
  V.foldr accRow (M.empty, M.empty, PQ.empty, PQ.empty, [], []) denseMap

accRow :: DenseRow -> MapAccumulator -> MapAccumulator
accRow row (myMap, oponentsMap, myQueue, oponentsQueue, myMissiles, oponentsMissiles) =
  let (myMapWithRow, oponentsMapWithRow, myQueue', oponentsQueue', myMissiles', oponentsMissiles') = makeRow myMap oponentsMap row
  in  (myMapWithRow,
       oponentsMapWithRow,
       PQ.union myQueue' myQueue,
       PQ.union oponentsQueue' oponentsQueue,
       myMissiles ++ myMissiles',
       oponentsMissiles ++ oponentsMissiles')

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
