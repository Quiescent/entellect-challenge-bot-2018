module Main where

import Interpretor (repl)

main :: IO ()
main = repl ( \ _ -> "")
