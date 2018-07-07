{-# LANGUAGE DeriveGeneric #-}
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
                    Row,
                    BuildingUnderConstruction,
                    ConstructionQueue)
  where

import Data.Aeson (decode,
                   FromJSON,
                   parseJSON,
                   withObject,
                   (.:))
import qualified Data.PQueue.Min as PQ
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap as M
import GHC.Generics (Generic(..))

data PlayerType =
  A | B deriving (Show, Generic, Eq)

instance FromJSON PlayerType

data Missile = Missile { xDisp  :: Int,
                         yDisp  :: Int }
  deriving (Show, Generic, Eq)

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
  deriving (Show, Generic, Eq)

instance FromJSON BuildingType

data Building = Building { integrity              :: Int,
                           weaponCooldownTimeLeft :: Int,
                           buildingType           :: BuildingType }
              deriving (Show, Eq)

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
type BuildingUnderConstruction = (Int, (Int, Int), Building)

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
                       score             :: Int,
                       towerMap          :: TowerMap,
                       constructionQueue :: ConstructionQueue,
                       ownedMissiles     :: [Missile] }
              deriving (Show, Eq)

data ScratchPlayer = ScratchPlayer { playerType :: PlayerType,
                                     energy'    :: Int,
                                     health'    :: Int,
                                     hitsTaken' :: Int,
                                     score'     :: Int }
                   deriving (Show, Generic, Eq)

instance FromJSON ScratchPlayer where
  parseJSON = withObject "ScratchPlayer" $ \ v -> do
    playerType' <- v .: "playerType"
    energy''    <- v .: "energy"
    health''    <- v .: "health"
    hitsTaken'' <- v .: "hitsTaken"
    score''     <- v .: "score"
    return $ ScratchPlayer playerType'
                           energy''
                           health''
                           hitsTaken''
                           score''

data GameState = GameState { me       :: Player,
                             oponent  :: Player }
  deriving (Show, Eq)

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
                         aHitsTaken
                         aScore),
           (ScratchPlayer _
                          bEnergy
                          bHealth
                          bHitsTaken
                          bScore))) = extractPlayers players'
    return (GameState (Player aEnergy
                               aHealth
                               aHitsTaken
                               aScore
                               myTowerMap
                               myQueue
                               myMissiles)
                       (Player bEnergy
                               bHealth
                               bHitsTaken
                               bScore
                               oponentsTowerMap
                               oponentsQueue
                               oponentsMissiles))

extractPlayers :: V.Vector ScratchPlayer -> (ScratchPlayer, ScratchPlayer)
extractPlayers players =
  let firstPlayer@(ScratchPlayer firstPlayerType _ _ _ _)  = players V.! 0
      secondPlayer = players V.! 1
  in if firstPlayerType == A
     then (firstPlayer,  secondPlayer)
     else (secondPlayer, firstPlayer)

type Row = M.IntMap Building

type TowerMap = M.IntMap Row

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

type RowAccumulator = (Row, Row, ConstructionQueue, ConstructionQueue, [Missile], [Missile])

makeRow :: DenseRow -> RowAccumulator
makeRow =
  V.foldr accCell (M.empty, M.empty, PQ.empty, PQ.empty, [], [])

accCell :: CellStateContainer -> RowAccumulator -> RowAccumulator
accCell (CellStateContainer x' y' _ buildings' missiles') row@(_, _, _, _, myMissiles, oponentsMissiles) =
  (myRow', oponentsRow', myQueue', oponentsQueue', myMissiles ++ myMissiles', oponentsMissiles ++ oponentsMissiles')
  where
    (myMissiles', oponentsMissiles') = splitMissiles missiles'
    (myRow', oponentsRow', myQueue', oponentsQueue', _, _) =
      if not $ V.null buildings'
      then accBuilding x' y' (buildings' V.! 0) row
      else row

accBuilding :: Int -> Int -> ScratchBuilding -> RowAccumulator -> RowAccumulator
accBuilding x' y' (ScratchBuilding int ctl wctl bt A) (row, b, queue, d, e, f) =
  let building' = (Building int wctl bt)
  in if ctl < 0
     then (M.insert (fromIntegral x') building' row, b, queue,                                      d, e, f)
     else (row,                                      b, PQ.insert (ctl, (x', y'), building') queue, d, e, f)
accBuilding x' y' (ScratchBuilding int ctl wctl bt B) (a, row, c, queue, e, f) =
  let building' = (Building int wctl bt)
  in if ctl < 0
     then (a, M.insert (fromIntegral x') building' row, c, queue,                                      e, f)
     else (a, row,                                      c, PQ.insert (ctl, (x', y'), building') queue, e, f)

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
  V.ifoldr accRow (M.empty, M.empty, PQ.empty, PQ.empty, [], []) denseMap

accRow :: Int -> DenseRow -> MapAccumulator -> MapAccumulator
accRow y' row (myMap, oponentsMap, myQueue, oponentsQueue, myMissiles, oponentsMissiles) =
  let (myRow, oponentsRow, myQueue', oponentsQueue', myMissiles', oponentsMissiles') = makeRow row
  in  (insertIfNotEmpty y' myRow       myMap,
       insertIfNotEmpty y' oponentsRow oponentsMap,
       PQ.union myQueue' myQueue,
       PQ.union oponentsQueue' oponentsQueue,
       myMissiles ++ myMissiles',
       oponentsMissiles ++ oponentsMissiles')

insertIfNotEmpty :: Int -> Row -> TowerMap -> TowerMap
insertIfNotEmpty key row towerMap' =
  if M.null row
  then towerMap'
  else M.insert key row towerMap'

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
             deriving (Eq)

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

repl :: (GameState -> Command) -> IO ()
repl evaluate = fmap evaluate readGameState >>= printCommand
