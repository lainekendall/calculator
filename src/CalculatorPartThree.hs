module CalculatorPartThree where

import Control.Applicative
import Evaluator
import ParserCombinators
import ParserModel

parseExpression :: Parser AST
parseExpression = try parseFullExpression <|> parseValueOnly

parseFullExpression :: Parser AST
parseFullExpression = do
  i1 <- parseValue
  op <- parseOperator
  MkAST op i1 <$> parseExpression

parseValue :: Parser AST
parseValue = fmap Value (ignoreWhitespace parseNumber)

parseValueOnly :: Parser AST
parseValueOnly = parseValue >>= \i -> parseEof >> return i -- is there a simpler way to do this?

parseOperator :: Parser Operator
parseOperator = ignoreWhitespace $ fmap opToFunc (oneOf "+-*")

opToFunc :: Char -> Operator
opToFunc '+' = Add
opToFunc '-' = Subtract
opToFunc '*' = Multiply
