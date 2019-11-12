module CalculatorPartOne where

import Control.Applicative
import Data.Char
import Data.List
import Evaluator
import ParserModel

parseExpression :: Parser AST
parseExpression = parseFullExpression <|> parseValue

parseFullExpression :: Parser AST
parseFullExpression = do
  parseSpace
  i1 <- parseValue
  parseSpace
  op <- parseOperator
  parseSpace
  i2 <- parseExpression
  parseSpace
  return $ MkAST op i1 i2

parseValue :: Parser AST
parseValue = Parser f
  where
    f "" = Nothing
    f s@(c:cs) =
      if isDigit c
        then Just (Value $ read $ takeWhile isDigit s, dropWhile isDigit s)
        else Nothing

parseSpace :: Parser ()
parseSpace = Parser f
  where
    f s = Just ((), dropWhile isSpace s)

parseOperator :: Parser Operator
parseOperator = Parser f
  where
    f "" = Nothing
    f (c:cs) =
      case c of
        '+' -> Just (Add, cs)
        '-' -> Just (Subtract, cs)
        '*' -> Just (Multiply, cs)
        '/' -> Just (Divide, cs)
        _ -> Nothing
