{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

module Interpretor (repl,
                    commandFilePath,
                    parseStateString,
                    emptyPlayer,
                    Player(..),
                    Missile,
                    BuildingType(..),
                    Building,
                    Command(..),
                    GameState(..))
  where

import Data.Aeson (decode,
                   FromJSON,
                   parseJSON,
                   withObject,
                   (.:))
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as UV
import qualified Data.ByteString.Lazy        as B
import Data.Bits
import Control.DeepSeq
import VectorIndex

import Buildings
import Coord
import BitSetMap

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

data Player = Player { energy                          :: !Int,
                       health                          :: !Int,
                       allTowers                       :: !BuildingPlacements,
                       allBuiltTowers                  :: !BuildingPlacements,
                       energyTowersUnderConstruction   :: !BuildingPlacements,
                       energyTowers                    :: !BuildingPlacements,
                       attackTowersUnderConstruction   :: !BuildingPlacements,
                       attack3Towers                   :: !BuildingPlacements,
                       attack2Towers                   :: !BuildingPlacements,
                       attack1Towers                   :: !BuildingPlacements,
                       attack0Towers                   :: !BuildingPlacements,
                       defenseTowersUnderConstruction2 :: !BuildingPlacements,
                       defenseTowersUnderConstruction1 :: !BuildingPlacements,
                       defenseTowersUnderConstruction0 :: !BuildingPlacements,
                       defense4Towers                  :: !BuildingPlacements,
                       defense3Towers                  :: !BuildingPlacements,
                       defense2Towers                  :: !BuildingPlacements,
                       defense1Towers                  :: !BuildingPlacements,
                       missiles0                       :: !Missiles,
                       missiles1                       :: !Missiles,
                       missiles2                       :: !Missiles,
                       missiles3                       :: !Missiles,
                       missilesOtherSide0              :: !Missiles,
                       missilesOtherSide1              :: !Missiles,
                       missilesOtherSide2              :: !Missiles,
                       missilesOtherSide3              :: !Missiles }
              deriving (Show)

instance Eq Player where
  (==) (Player energyA
               healthA
               allTowersA
               allBuiltTowersA
               energyTowersUnderConstructionA
               energyTowersA
               attackTowersUnderConstructionA
               attack3TowersA
               attack2TowersA
               attack1TowersA
               attack0TowersA
               defenseTowersUnderConstruction2A
               defenseTowersUnderConstruction1A
               defenseTowersUnderConstruction0A
               defenseTowers4A
               defenseTowers3A
               defenseTowers2A
               defenseTowers1A
               missiles0A
               missiles1A
               missiles2A
               missiles3A
               missilesOtherSide0A
               missilesOtherSide1A
               missilesOtherSide2A
               missilesOtherSide3A)
       (Player energyB
               healthB
               allTowersB
               allBuiltTowersB
               energyTowersUnderConstructionB
               energyTowersB
               attackTowersUnderConstructionB
               attack3TowersB
               attack2TowersB
               attack1TowersB
               attack0TowersB
               defenseTowersUnderConstruction2B
               defenseTowersUnderConstruction1B
               defenseTowersUnderConstruction0B
               defenseTowers4B
               defenseTowers3B
               defenseTowers2B
               defenseTowers1B
               missiles0B
               missiles1B
               missiles2B
               missiles3B
               missilesOtherSide0B
               missilesOtherSide1B
               missilesOtherSide2B
               missilesOtherSide3B)
    = energyA                          == energyB &&
      healthA                          == healthB &&
      allTowersA                       == allTowersB &&
      allBuiltTowersA                  == allBuiltTowersB &&
      energyTowersUnderConstructionA   == energyTowersUnderConstructionB &&
      energyTowersA                    == energyTowersB &&
      attackTowersUnderConstructionA   == attackTowersUnderConstructionB &&
      attack3TowersA                   == attack3TowersB &&
      attack2TowersA                   == attack2TowersB &&
      attack1TowersA                   == attack1TowersB &&
      attack0TowersA                   == attack0TowersB &&
      defenseTowersUnderConstruction2A == defenseTowersUnderConstruction2B &&
      defenseTowersUnderConstruction1A == defenseTowersUnderConstruction1B &&
      defenseTowersUnderConstruction0A == defenseTowersUnderConstruction0B &&
      defenseTowers4A                  == defenseTowers4B &&
      defenseTowers3A                  == defenseTowers3B &&
      defenseTowers2A                  == defenseTowers2B &&
      defenseTowers1A                  == defenseTowers1B &&
      missiles0A `xor` missiles1A `xor` missiles2A `xor` missiles3A ==
      missiles0B `xor` missiles1B `xor` missiles2B `xor` missiles3B &&
      missilesOtherSide0B `xor` missilesOtherSide1B `xor` missilesOtherSide2B `xor` missilesOtherSide3B ==
      missilesOtherSide0A `xor` missilesOtherSide1A `xor` missilesOtherSide2A `xor` missilesOtherSide3A

instance NFData Player where
  rnf (Player energy'
              health'
              allTowers'
              allBuiltTowers'
              energyTowersUnderConstruction'
              energyTowers'
              attackTowersUnderConstruction'
              attack3Towers'
              attack2Towers'
              attack1Towers'
              attack0Towers'
              defenseTowersUnderConstruction2'
              defenseTowersUnderConstruction1'
              defenseTowersUnderConstruction0'
              defense4Towers'
              defense3Towers'
              defense2Towers'
              defense1Towers'
              missiles0'
              missiles1'
              missiles2'
              missiles3'
              missilesOtherSide0'
              missilesOtherSide1'
              missilesOtherSide2'
              missilesOtherSide3')
    = energy'                          `seq`
      health'                          `seq`
      allTowers'                       `seq`
      allBuiltTowers'                  `seq`
      energyTowersUnderConstruction'   `seq`
      energyTowers'                    `seq`
      attackTowersUnderConstruction'   `seq`
      attack3Towers'                   `seq`
      attack2Towers'                   `seq`
      attack1Towers'                   `seq`
      attack0Towers'                   `seq`
      defenseTowersUnderConstruction2' `seq`
      defenseTowersUnderConstruction1' `seq`
      defenseTowersUnderConstruction0' `seq`
      defense4Towers'                  `seq`
      defense3Towers'                  `seq`
      defense2Towers'                  `seq`
      defense1Towers'                  `seq`
      missiles0'                       `seq`
      missiles1'                       `seq`
      missiles2'                       `seq`
      missiles3'                       `seq`
      missilesOtherSide0'              `seq`
      missilesOtherSide1'              `seq`
      missilesOtherSide2'              `seq`
      missilesOtherSide3'              `seq`
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

data GameDetails = GameDetails { gameRound' :: Int }

instance FromJSON GameDetails where
  parseJSON = withObject "GameDetails" $ \ v -> do
    gameRound'' <- v.: "round"
    return $ GameDetails gameRound''

data GameState = GameState { gameRound :: Int,
                             me        :: Player,
                             oponent   :: Player }
  deriving (Show, Eq)

instance NFData GameState where
  rnf (GameState gameRound'' me' oponent') =
    (rnf gameRound'') `seq`
    (rnf me')         `seq`
    (rnf oponent')    `seq`
    ()

instance FromJSON GameState where
  parseJSON = withObject "GameState" $ \ v -> do
    players'      <- v .: "players"
    denseGameMap  <- v .: "gameMap"
    gameDetails   <- v .: "gameDetails"
    let (GameState _ me' oponent') = convertDenseMap denseGameMap
    let (((ScratchPlayer _
                         aEnergy
                         aHealth),
           (ScratchPlayer _
                          bEnergy
                          bHealth))) = extractPlayers players'
    return (GameState (gameRound' gameDetails)
                      me' { energy             = aEnergy,
                            health             = aHealth }
                      oponent' { energy             = bEnergy,
                                 health             = bHealth })

extractPlayers :: V.Vector ScratchPlayer -> (ScratchPlayer, ScratchPlayer)
extractPlayers players =
  let firstPlayer@(ScratchPlayer firstPlayerType _ _)  = players `vectorIndex` 0
      secondPlayer = players `vectorIndex` 1
  in if firstPlayerType == "A"
     then (firstPlayer,  secondPlayer)
     else (secondPlayer, firstPlayer)

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
emptyPlayer = Player
  0
  0
  emptyBuildings
  emptyBuildings
  emptyBuildings
  emptyBuildings
  emptyBuildings
  emptyBuildings
  emptyBuildings
  emptyBuildings
  emptyBuildings
  emptyBuildings
  emptyBuildings
  emptyBuildings
  emptyBuildings
  emptyBuildings
  emptyBuildings
  emptyBuildings
  emptyMissiles
  emptyMissiles
  emptyMissiles
  emptyMissiles
  emptyMissiles
  emptyMissiles
  emptyMissiles
  emptyMissiles

emptyGameState :: GameState
emptyGameState = GameState 0 emptyPlayer emptyPlayer

convertDenseMap :: DenseMap -> GameState
convertDenseMap = V.foldr accRow emptyGameState

accRow :: DenseRow -> GameState -> GameState
accRow = flip (V.foldr accCell)

accCell :: CellStateContainer -> GameState -> GameState
accCell (CellStateContainer x' y' _ buildings' missiles') =
  accMissiles missiles' . accBuildings x' y' buildings'

accMissiles :: V.Vector ScratchMissile -> GameState -> GameState
accMissiles missiles' gameState@(GameState _ me' oponent') =
  gameState { me      = accMissilesToMyPlayer       myMissiles       me',
              oponent = accMissilesToOponentsPlayer oponentsMissiles oponent' }
  where
    (myMissiles, oponentsMissiles) = splitMissiles missiles'

accMissilesToMyPlayer :: UV.Vector (Int, Int) -> Player -> Player
accMissilesToMyPlayer missiles player =
  UV.foldr addMissileToMyPlayer player missiles

addMissileToMyPlayer :: (Int, Int) -> Player -> Player
addMissileToMyPlayer (x', y') player =
  let missileToAdd = addMissile (toCoord x' y') 0
  in if x' < halfWay
     then addMissilesToThisSide  missileToAdd player
     else addMissilesToOtherSide missileToAdd player

accMissilesToOponentsPlayer :: UV.Vector (Int, Int) -> Player -> Player
accMissilesToOponentsPlayer missiles player =
  UV.foldr addMissileToOponentsPlayer player missiles

addMissileToOponentsPlayer :: (Int, Int) -> Player -> Player
addMissileToOponentsPlayer (x', y') player =
  let missileToAdd = addMissile (toCoord x' y') 0
  in if x' >= halfWay
     then addMissilesToThisSide  missileToAdd player
     else addMissilesToOtherSide missileToAdd player

addMissilesToThisSide :: Missiles -> Player -> Player
addMissilesToThisSide missilesToAddTo0
  player@(Player { missiles0 = missiles0',
                   missiles1 = missiles1',
                   missiles2 = missiles2',
                   missiles3 = missiles3' }) =
  let missilesOn0      = addAllMissiles missilesToAddTo0 missiles0'
      missilesToAddTo1 = onlyOverlappingMissiles missilesToAddTo0 missiles0'
      missilesOn1      = addAllMissiles missilesToAddTo1 missiles1'
      missilesToAddTo2 = onlyOverlappingMissiles missilesToAddTo1 missiles1'
      missilesOn2      = addAllMissiles missilesToAddTo2 missiles2'
      missilesToAddTo3 = onlyOverlappingMissiles missilesToAddTo2 missiles2'
      missilesOn3      = addAllMissiles missilesToAddTo3 missiles3'
  in player { missiles0 = missilesOn0,
              missiles1 = missilesOn1,
              missiles2 = missilesOn2,
              missiles3 = missilesOn3 }

addMissilesToOtherSide :: Missiles -> Player -> Player
addMissilesToOtherSide missilesToAddTo0
  player@(Player { missilesOtherSide0 = missilesOtherSide0',
                   missilesOtherSide1 = missilesOtherSide1',
                   missilesOtherSide2 = missilesOtherSide2',
                   missilesOtherSide3 = missilesOtherSide3' }) =
  let missilesOn0      = addAllMissiles missilesToAddTo0 missilesOtherSide0'
      missilesToAddTo1 = onlyOverlappingMissiles missilesToAddTo0 missilesOtherSide0'
      missilesOn1      = addAllMissiles missilesToAddTo1 missilesOtherSide1'
      missilesToAddTo2 = onlyOverlappingMissiles missilesToAddTo1 missilesOtherSide1'
      missilesOn2      = addAllMissiles missilesToAddTo2 missilesOtherSide2'
      missilesToAddTo3 = onlyOverlappingMissiles missilesToAddTo2 missilesOtherSide2'
      missilesOn3      = addAllMissiles missilesToAddTo3 missilesOtherSide3'
  in player { missilesOtherSide0 = missilesOn0,
              missilesOtherSide1 = missilesOn1,
              missilesOtherSide2 = missilesOn2,
              missilesOtherSide3 = missilesOn3 }

splitMissiles :: V.Vector ScratchMissile -> (UV.Vector (Int, Int), UV.Vector (Int, Int))
splitMissiles = V.foldr splitMissilesAcc (UV.empty, UV.empty)

splitMissilesAcc :: ScratchMissile -> (UV.Vector (Int, Int), UV.Vector (Int, Int)) -> (UV.Vector (Int, Int), UV.Vector (Int, Int))
splitMissilesAcc (ScratchMissile _ _ owner' x' y') (myMissiles, oponentsMissiles) =
  if owner' == "A"
  then (UV.cons (x', y') myMissiles, oponentsMissiles)
  else (myMissiles,                  UV.cons (x', y') oponentsMissiles)

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
accBuilding _ _ _ building' = error $ "Building belongs to neither Player A or B: " ++ show building'

toBuildingType :: String -> BuildingType
toBuildingType "DEFENSE" = DEFENSE
toBuildingType "ATTACK"  = ATTACK
toBuildingType "ENERGY"  = ENERGY
toBuildingType "TESLA"   = TESLA
toBuildingType x         = error $ "Unknown building type: " ++ x

accBuildingToPlayer :: Int -> Int -> ScratchBuilding -> Player -> Player
accBuildingToPlayer x' y' (ScratchBuilding int ctl wctl bt _)
  player@(Player { allTowers                       = allTowers',
                   allBuiltTowers                  = allBuiltTowers', 
                   energyTowersUnderConstruction   = energyTowersUnderConstruction',
                   energyTowers                    = energyTowers',
                   attackTowersUnderConstruction   = attackTowersUnderConstruction',
                   attack3Towers                   = attack3Towers',
                   attack2Towers                   = attack2Towers',
                   attack1Towers                   = attack1Towers',
                   attack0Towers                   = attack0Towers',
                   defenseTowersUnderConstruction2 = defenseTowersUnderConstruction2',
                   defenseTowersUnderConstruction1 = defenseTowersUnderConstruction1',
                   defenseTowersUnderConstruction0 = defenseTowersUnderConstruction0',
                   defense4Towers                  = defense4Towers',
                   defense3Towers                  = defense3Towers',
                   defense2Towers                  = defense2Towers',
                   defense1Towers                  = defense1Towers' }) =
  let building'        = chooseBuilding int wctl (toBuildingType bt)
      coord'           = toCoord x' y'
      allTowers''      = addBuilding coord' allTowers'
      allBuiltTowers'' = if ctl < 0 then addBuilding coord' allBuiltTowers' else allBuiltTowers'
      player'          = player { allTowers      = allTowers'',
                                  allBuiltTowers = allBuiltTowers'' }
  in case (ctl < 0, ctl, building') of
       (True,  _, Attack3)       -> player' { attack3Towers  = addBuilding coord' attack3Towers' }
       (True,  _, Attack2)       -> player' { attack2Towers  = addBuilding coord' attack2Towers' }
       (True,  _, Attack1)       -> player' { attack1Towers  = addBuilding coord' attack1Towers' }
       (True,  _, Attack0)       -> player' { attack0Towers  = addBuilding coord' attack0Towers' }
       (True,  _, Defense4)      -> player' { defense4Towers = addBuilding coord' defense4Towers' }
       (True,  _, Defense3)      -> player' { defense3Towers = addBuilding coord' defense3Towers' }
       (True,  _, Defense2)      -> player' { defense2Towers = addBuilding coord' defense2Towers' }
       (True,  _, Defense1)      -> player' { defense1Towers = addBuilding coord' defense1Towers' }
       (True,  _, EnergyTower)   -> player' { energyTowers   = addBuilding coord' energyTowers' }
       -- Under Construction:
       (False, 2, Defense4)      -> player' { defenseTowersUnderConstruction2 =
                                               addBuilding coord' defenseTowersUnderConstruction2' }
       (False, 1, Defense4)      -> player' { defenseTowersUnderConstruction1 =
                                               addBuilding coord' defenseTowersUnderConstruction1' }
       (False, 0, Defense4)      -> player' { defenseTowersUnderConstruction0 =
                                               addBuilding coord' defenseTowersUnderConstruction0' }
       (False, _, Attack0)       -> player' { attackTowersUnderConstruction =
                                              addBuilding coord' attackTowersUnderConstruction' }
       (False, _, EnergyTower)   -> player' { energyTowersUnderConstruction =
                                              addBuilding coord' energyTowersUnderConstruction' }
       (_,     _, _)             -> error "Invalid building"

chooseBuilding :: Int -> Int -> BuildingType -> Building

chooseBuilding _  _ ENERGY = EnergyTower

chooseBuilding _ 3 ATTACK = Attack3
chooseBuilding _ 2 ATTACK = Attack2
chooseBuilding _ 1 ATTACK = Attack1
chooseBuilding _ 0 ATTACK = Attack0

chooseBuilding 20 _ DEFENSE = Defense4
chooseBuilding 15 _ DEFENSE = Defense3
chooseBuilding 10 _ DEFENSE = Defense2
chooseBuilding 5  _ DEFENSE = Defense1

chooseBuilding _ 10 TESLA = Tesla10
chooseBuilding _ 9  TESLA = Tesla9
chooseBuilding _ 8  TESLA = Tesla8
chooseBuilding _ 7  TESLA = Tesla7
chooseBuilding _ 6  TESLA = Tesla6
chooseBuilding _ 5  TESLA = Tesla5
chooseBuilding _ 4  TESLA = Tesla4
chooseBuilding _ 3  TESLA = Tesla3
chooseBuilding _ 2  TESLA = Tesla2
chooseBuilding _ 1  TESLA = Tesla1
chooseBuilding _ 0  TESLA = Tesla0

chooseBuilding int ctl' buildingType' =
  error $ "(int, ctl, buildingType) not recognised: " ++ show (int, ctl', buildingType')

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
               | IronCurtain
               | Deconstruct { dCoord :: Coord }
               | NothingCommand
             deriving (Eq)

instance NFData Command where
  rnf (Build bCoord' building') =
    bCoord'   `seq`
    building' `seq`
    ()
  rnf IronCurtain = ()
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
  show IronCurtain = "5"
  show (Deconstruct dCoord') =
    let (x', y') = fromCoord dCoord'
    in show x' ++ "," ++ show y' ++ ","  ++ "3"
  show NothingCommand = ""

repl :: (GameState -> IO ()) -> IO ()
repl evaluate = readGameState >>= evaluate
