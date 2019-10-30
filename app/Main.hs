module Main where

import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import CalculatorPartOne
import Control.Monad (forever)
import ParserModel
import Evaluator

main :: IO ()
main = forever $ do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to the Calculator! Part One..."
  putStr "> "
  input <- getLine
  print $ evaluate $ runParser parseExpression input
