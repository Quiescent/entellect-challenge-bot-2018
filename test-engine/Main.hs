module Main
  where

import Interpretor
import Engine
import GameState
import Magic
import Coord
import EfficientCommand

import Text.Printf
import System.Environment
import System.Directory
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B

-- Parse initial state
-- iterate through states and moves
-- compare game states

main :: IO ()
main = do
  args              <- getArgs
  let playerOneName  = args !! 0
  let playerTwoName  = args !! 1
  let matchDirectory = args !! 2
  let playerOneDir   = (matchDirectory ++) . roundToPlayerOneDirectory playerOneName
  let playerTwoDir   = (matchDirectory ++) . roundToPlayerTwoDirectory playerTwoName
  let playMoveForRound = \ currentState round' -> do
        putStrLn $ "Round: " ++ show round'
        playerOneMove <- readPlayerCommand $ playerOneDir round'
        putStrLn $ "Player 1's move: " ++ show playerOneMove
        playerTwoMove <- fmap transposePlayerTwosMove $ readPlayerCommand $ playerTwoDir round'
        putStrLn $ "Player 2's move: " ++ show playerTwoMove
        reportedState <- parseState $ playerOneDir (round' + 1)
        let updatedState = updateMyMove (toEfficientCommand playerOneMove) $ updateOponentsMove (toEfficientCommand playerTwoMove) $ tickEngine currentState
        if reportedState /= updatedState
          then error $ "On round: " ++ show round' ++ "\nExpected:\t" ++ show reportedState ++ "\nGot:\t\t" ++ show updatedState
          else return updatedState
  roundCount        <- countRounds matchDirectory
  initialState      <- parseState $ playerOneDir 0
  putStrLn $ playerOneName ++ ", " ++
             playerTwoName ++ ", " ++
             matchDirectory
  foldM_ playMoveForRound initialState [0..roundCount - 2]
  putStrLn "Done"

transposePlayerTwosMove :: Command -> Command
transposePlayerTwosMove (Build       coord' buildingType) =
  let (x, y) = fromCoord coord'
  in (Build       (toCoord (width - x - 1) y) buildingType)
transposePlayerTwosMove (Deconstruct coord')              =
  let (x, y) = fromCoord coord'
  in (Deconstruct (toCoord (width - x - 1) y))
transposePlayerTwosMove x                                 = x

parseState :: String -> IO GameState
parseState playerRoundDirectory =
  fmap parseStateString $ B.readFile $ playerRoundDirectory ++ "JsonMap.json"

countRounds :: String -> IO Int
countRounds baseGameDirectory =
  fmap length $ listDirectory baseGameDirectory

-- Convention: Directories end in a slash.

roundToPlayerOneDirectory :: String -> Int -> String
roundToPlayerOneDirectory playerOneName round' =
  "Round " ++ printf "%03d" round' ++ "/A - " ++ playerOneName ++ "/"

roundToPlayerTwoDirectory :: String -> Int -> String
roundToPlayerTwoDirectory playerTwoName round' =
  "Round " ++ printf "%03d" round' ++ "/B - " ++ playerTwoName ++ "/"

readPlayerCommand :: String -> IO Command
readPlayerCommand directory = do
  fileContents <- readFile $ directory ++ "PlayerCommand.txt"
  if fileContents == "No Command" || fileContents == ""
    then return NothingCommand
    else let (xStr, rest)  = span (/= ',') fileContents
             x             = read xStr
             (yStr, rest') = span (/= ',') $ tail rest
             y             = read yStr
             coord         = toCoord x y
             bstr          = tail rest'
         in return $ case bstr of
                       "0" -> Build coord DEFENSE
                       "1" -> Build coord ATTACK
                       "2" -> Build coord ENERGY
                       "3" -> Deconstruct coord
                       "4" -> Build coord TESLA
                       _   -> error ("Read unhandled building type: " ++ bstr)
