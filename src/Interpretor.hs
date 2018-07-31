{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Interpretor (repl,
                    parseStateString,
                    incrementFitness,
                    decrementFitness,
                    insertMissileSortedForMe,
                    insertMissileSortedForOponent,
                    Player(..),
                    Missile(..),
                    BuildingType(..),
                    Building,
                    Command(..),
                    GameState(..),
                    TowerMap,
                    BuildingUnderConstruction,
                    ConstructionQueue,
                    Missiles)
  where

import Data.Aeson (decode,
                   FromJSON,
                   parseJSON,
                   withObject,
                   (.:))
import qualified Data.PQueue.Min      as PQ
import qualified Data.Vector          as V
import qualified Data.Vector.Unboxed  as UV
import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap.Strict   as M
import qualified Data.List            as L
import Control.DeepSeq
import VectorIndex

import Buildings
import Coord
import Magic

type Missile = Coord

data ScratchMissile = ScratchMissile Int
                                     Int
                                     String
                                     Int
                                     Int
  deriving (Show, Eq)

instance FromJSON ScratchMissile where
  parseJSON = withObject "Missile" $ \ v -> do
    damage'     <- v.: "damage"
    speed'      <- v.: "speed"
    playerType' <- v.: "playerType"
    x'          <- v.: "x"
    y'          <- v.: "y"
    return $ ScratchMissile damage' speed' playerType' x' y'

data BuildingType = DEFENSE | ATTACK | ENERGY | TESLA
  deriving (Show, Eq, Ord)

instance NFData BuildingType where
  rnf bt = bt `seq` ()

data ScratchBuilding = ScratchBuilding Int
                                       Int
                                       Int
                                       String
                                       String
                deriving (Show, Eq)

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

type Missiles = UV.Vector Missile

data Player = Player { energy            :: Int,
                       health            :: Int,
                       energyGenPerTurn  :: Int,
                       towerMap          :: TowerMap,
                       constructionQueue :: ConstructionQueue,
                       ownedMissiles     :: Missiles }
              deriving (Show)

-- Allows for built in sorting
toOrderedBuildingUnderConstruction :: BuildingUnderConstruction -> (Int, Int, Coord, Building)
toOrderedBuildingUnderConstruction (x, y, z) = (0, x, y, z)

compareFullyOrderedConstruction :: BuildingUnderConstruction -> BuildingUnderConstruction -> Ordering
compareFullyOrderedConstruction x y = compare (toOrderedBuildingUnderConstruction x) (toOrderedBuildingUnderConstruction y)

instance Eq Player where
  (==) (Player energyA healthA  energyGenPerTurnA towerMapA constructionQueueA ownedMissilesA)
       (Player energyB healthB energyGenPerTurnB towerMapB constructionQueueB ownedMissilesB)
    = energyA                             == energyB &&
      healthA                             == healthB &&
      energyGenPerTurnA                   == energyGenPerTurnB &&
      towerMapA                           == towerMapB &&
      (L.sort $ UV.toList ownedMissilesA) == (L.sort $ UV.toList ownedMissilesB) &&
      (L.sortBy compareFullyOrderedConstruction $ PQ.toList constructionQueueA) == (L.sortBy compareFullyOrderedConstruction $ PQ.toList constructionQueueB)

instance NFData Player where
  rnf (Player energy'
              health'
              energyGenPerTurn'
              towerMap'
              constructionQueue'
              ownedMissiles')
    = energy'                  `seq`
      health'                  `seq`
      energyGenPerTurn'        `seq`
      (rnf towerMap')          `seq`
      (rnf constructionQueue') `seq`
      (rnf ownedMissiles')     `seq`
      ()

data ScratchPlayer = ScratchPlayer String Int Int
                   deriving (Show, Eq)

instance FromJSON ScratchPlayer where
  parseJSON = withObject "ScratchPlayer" $ \ v -> do
    playerType' <- v .: "playerType"
    energy''    <- v .: "energy"
    health''    <- v .: "health"
    return $ ScratchPlayer playerType'
                           energy''
                           health''

data GameState = GameState { me       :: Player,
                             oponent  :: Player }
  deriving (Show, Eq)

instance NFData GameState where
  rnf (GameState me' oponent') =
    (rnf me)      `seq`
    (rnf oponent) `seq`
    ()

instance FromJSON GameState where
  parseJSON = withObject "GameState" $ \ v -> do
    players'      <- v .: "players"
    denseGameMap  <- v .: "gameMap"
    let (GameState me' oponent') = convertDenseMap denseGameMap
    let (((ScratchPlayer _
                         aEnergy
                         aHealth),
           (ScratchPlayer _
                          bEnergy
                          bHealth))) = extractPlayers players'
    return (GameState me' { energy    = aEnergy,
                            health    = aHealth }
                      oponent' { energy    = bEnergy,
                                 health    = bHealth })

extractPlayers :: V.Vector ScratchPlayer -> (ScratchPlayer, ScratchPlayer)
extractPlayers players =
  let firstPlayer@(ScratchPlayer firstPlayerType _ _)  = players `vectorIndex` 0
      secondPlayer = players `vectorIndex` 1
  in if firstPlayerType == "A"
     then (firstPlayer,  secondPlayer)
     else (secondPlayer, firstPlayer)

type TowerMap = M.IntMap Building

data CellStateContainer = CellStateContainer Int
                                             Int
                                             String
                                             (V.Vector ScratchBuilding)
                                             (V.Vector ScratchMissile)
                          deriving (Show, Eq)

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
emptyPlayer = Player 0 0 0 M.empty PQ.empty UV.empty

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
  gameState { me      = me'      { ownedMissiles = myExistingMissiles       UV.++ myMissiles },
              oponent = oponent' { ownedMissiles = oponentsExistingMissiles UV.++ oponentsMissiles } }
  where
    myExistingMissiles             = ownedMissiles me'
    oponentsExistingMissiles       = ownedMissiles oponent'
    (myMissiles, oponentsMissiles) = (splitMissiles missiles')

splitMissiles :: V.Vector ScratchMissile -> (Missiles, Missiles)
splitMissiles = V.foldr splitMissilesAcc (UV.empty, UV.empty)

insertMissileSortedForOponent :: Missile -> Missiles -> Missiles
insertMissileSortedForOponent missile missiles =
  let (lte, ge) = UV.span (<= missile) missiles
  in UV.concat [lte, UV.singleton missile, ge]

insertMissileSortedForMe :: Missile -> Missiles -> Missiles
insertMissileSortedForMe missile missiles =
  let (lte, ge) = UV.span (>= missile) missiles
  in UV.concat [lte, UV.singleton missile, ge]

splitMissilesAcc :: ScratchMissile -> (Missiles, Missiles) -> (Missiles, Missiles)
splitMissilesAcc (ScratchMissile _ _ owner' x' y') (myMissiles, oponentsMissiles) =
  let missile = (toCoord x' y')
  in if owner' == "A"
     then (UV.cons missile myMissiles, oponentsMissiles)
     else (myMissiles,                 UV.cons missile oponentsMissiles)

accBuildings :: Int -> Int -> V.Vector ScratchBuilding -> GameState -> GameState
accBuildings x' y' buildings' =
  if not $ V.null buildings'
  then accBuilding x' y' (buildings' `vectorIndex` 0)
  else id

accBuilding :: Int -> Int -> ScratchBuilding -> GameState -> GameState
accBuilding x' y' building'@(ScratchBuilding _ _ _ _ "A") state =
  state { me = accBuildingToPlayer x' y' building' (me state) }
accBuilding x' y' building'@(ScratchBuilding _ _ _ _ "B") state =
  state { oponent = accBuildingToPlayer x' y' building' (oponent state) }

toBuildingType :: String -> BuildingType
toBuildingType "DEFENSE" = DEFENSE
toBuildingType "ATTACK"  = ATTACK
toBuildingType "ENERGY"  = ENERGY
toBuildingType "TESLA"   = TESLA

accBuildingToPlayer :: Int -> Int -> ScratchBuilding -> Player -> Player
accBuildingToPlayer x' y' (ScratchBuilding int ctl wctl bt _) player@(Player { towerMap          = towerMap',
                                                                               constructionQueue = constructionQueue' }) =
  let building' = chooseBuilding int wctl (toBuildingType bt)
  in incrementFitness y' building' $
     if ctl < 0
     then player { towerMap = M.insert (toCoord x' y') building' towerMap' }
     else player { constructionQueue = PQ.insert (ctl, toCoord x' y', building') constructionQueue' }

chooseBuilding :: Int -> Int -> BuildingType -> Building

chooseBuilding _  _ ENERGY = energyTower

chooseBuilding _ 3 ATTACK = attack3
chooseBuilding _ 2 ATTACK = attack2
chooseBuilding _ 1 ATTACK = attack1
chooseBuilding _ 0 ATTACK = attack0

chooseBuilding 20 _ DEFENSE = defense4
chooseBuilding 15 _ DEFENSE = defense3
chooseBuilding 10 _ DEFENSE = defense2
chooseBuilding 5  _ DEFENSE = defense1

chooseBuilding _ 10 TESLA = tesla10
chooseBuilding _ 9  TESLA = tesla9
chooseBuilding _ 8  TESLA = tesla8
chooseBuilding _ 7  TESLA = tesla7
chooseBuilding _ 6  TESLA = tesla6
chooseBuilding _ 5  TESLA = tesla5
chooseBuilding _ 4  TESLA = tesla4
chooseBuilding _ 3  TESLA = tesla3
chooseBuilding _ 2  TESLA = tesla2
chooseBuilding _ 1  TESLA = tesla1
chooseBuilding _ 0  TESLA = tesla0

missileDamagePerTurn :: Float
missileDamagePerTurn = (fromIntegral missileDamage) / (fromIntegral attackTowerCooldownTime)

teslaTowerDamagePerTurn :: Float
teslaTowerDamagePerTurn = (fromIntegral teslaTowerMaximumHitDamage) / (fromIntegral teslaTowerCooldownTime)

incrementFitness :: Int -> Building -> Player -> Player
incrementFitness y' building'  player@(Player { energyGenPerTurn = energyGenPerTurn' })
  | building' == attack3     = player
  | building' == attack2     = player
  | building' == attack1     = player
  | building' == attack0     = player
  | building' == defense4    = player
  | building' == defense3    = player
  | building' == defense2    = player
  | building' == defense1    = player
  | building' == energyTower = player { energyGenPerTurn = energyGenPerTurn' + energyTowerEnergyGeneratedPerTurn }
  -- TODO: Come up with something reasonable here
  | otherwise                = player

decrementFitness :: Int -> Building -> Player -> Player
decrementFitness y' building' player@(Player { energyGenPerTurn = energyGenPerTurn' })
  | building' == attack3     = player
  | building' == attack2     = player
  | building' == attack1     = player
  | building' == attack0     = player
  | building' == defense4    = player
  | building' == defense3    = player
  | building' == defense2    = player
  | building' == defense1    = player
  | building' == energyTower = player { energyGenPerTurn = energyGenPerTurn' - energyTowerEnergyGeneratedPerTurn  }
  | otherwise                = player

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

data Command = Build { bCoord   :: Coord,
                       building :: BuildingType }
               | Deconstruct { dCoord :: Coord }
               | NothingCommand
             deriving (Eq)

instance NFData Command where
  rnf (Build bCoord' building') =
    bCoord'   `seq`
    building' `seq`
    ()
  rnf (Deconstruct dCoord') =
    dCoord' `seq` ()
  rnf cmd@(NothingCommand) =
    cmd `seq` ()

instance Show Command where
  show (Build bCoord' building') =
    let (x', y') = fromCoord bCoord'
    in show x' ++ "," ++ show y' ++ ","  ++ case building' of
      DEFENSE -> "0"
      ATTACK  -> "1"
      ENERGY  -> "2"
      TESLA   -> "4"
  show (Deconstruct dCoord') =
    let (x', y') = fromCoord dCoord'
    in show x' ++ "," ++ show y' ++ ","  ++ "3"
  show NothingCommand = ""

printCommand :: Command ->  IO ()
printCommand = writeFile commandFilePath . show

repl :: (GameState -> IO Command) -> IO ()
repl evaluate = readGameState >>= evaluate >>= printCommand
