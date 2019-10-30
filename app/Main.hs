module Main where

import CalculatorPartOne
import Control.Monad (forever)
import Evaluator
import ParserModel
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to the Calculator! Part One..."
  forever $ do
    putStr "> "
    input <- getLine
    print $ evaluate $ runParser parseExpression input
