module CalculatorPartOne where

import Data.Char
import Data.List
import ParserModel
import Evaluator

parseExpression :: Parser AST
parseExpression = do
  i1 <- parseValue 
  parseSpace
  op <- parseOperator
  parseSpace
  i2 <- parseValue
  return $ op i1 i2

parseValue :: Parser AST
parseValue = Parser f
  where
    f "" = Nothing
    f s@(c:cs) = if isDigit c 
                 then Just (Value $ read $ takeWhile isDigit s, dropWhile isDigit s)
                 else Nothing

parseSpace :: Parser ()
parseSpace = Parser f
  where 
    f "" = Nothing
    f s = Just ((), dropWhile isSpace s)

parseOperator :: Parser (AST -> AST -> AST)
parseOperator = Parser f
  where
    f "" = Nothing
    f (c:cs) = case c of
                 '+' -> Just ( Add, cs)
                 '-' -> Just ( Subtract, cs)
                 '*' -> Just ( Multiply, cs)
                 '/' -> Just ( Divide, cs)
