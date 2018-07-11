module Main
  where

import Interpretor
import Engine
import GameState
import Magic
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
        playerTwoMove <- fmap transposePlayerTwosMove $ readPlayerCommand $ playerTwoDir round'
        reportedState <- parseState $ playerOneDir (round' + 1)
        let updatedState = updateMyMove playerOneMove $ updateOponentsMove playerTwoMove $ tickEngine currentState
        if reportedState /= updatedState
          then error $ "On round: " ++ show round' ++ "\nExpected:\t" ++ show reportedState ++ "\nGot:\t\t" ++ show updatedState
          else return updatedState
  roundCount        <- countRounds matchDirectory
  initialState      <- parseState $ playerOneDir 0
  putStrLn $ playerOneName ++ ", " ++
             playerTwoName ++ ", " ++
             matchDirectory
  foldM playMoveForRound initialState [0..roundCount - 2]
  putStrLn "Done"

transposePlayerTwosMove :: Command -> Command
transposePlayerTwosMove (Build       x y buildingType) = (Build       (width - x - 1) y buildingType)
transposePlayerTwosMove (Deconstruct x y)              = (Deconstruct (width - x - 1) y)
transposePlayerTwosMove x                              = x

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
             bstr          = tail rest'
         in return $ case bstr of
                       "0" -> Build x y DEFENSE
                       "1" -> Build x y ATTACK
                       "2" -> Build x y ENERGY
                       "3" -> Deconstruct x y
                       "4" -> Build x y TESLA
                       _   -> error ("Read unhandled building type: " ++ bstr)
