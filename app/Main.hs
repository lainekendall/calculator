module Main where

import qualified CalculatorPartOne as One
import qualified CalculatorPartTwo as Two
import Control.Monad (forever)
import Evaluator
import ParserModel
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to the Calculator! Which version do you want to try (1/2?)..."
  version <- getLine
  let parser = getParser version
  forever $ do
    putStr "> "
    input <- getLine
    print $ evaluate $ runParser parser input

getParser :: String -> Parser AST
getParser "1" = One.parseExpression
getParser "2" = Two.parseExpression
getParser _ = error "Incorrect version"
