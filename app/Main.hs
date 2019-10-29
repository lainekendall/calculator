module Main where

import CalculatorPartOne
import Control.Monad (forever)
import ParserModel
import Evaluator

main :: IO ()
main = forever $ do
  putStrLn "Welcome to the Calculator! Part One..."
  putStr "> "
  input <- getLine
  print $ evaluate $ runParser parseExpression input
