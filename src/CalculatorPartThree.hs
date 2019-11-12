module CalculatorPartThree where

import Evaluator
import Text.Parsec

type Parser = Parsec String ()

runParser :: Parsec String () a -> String -> Either ParseError a
runParser p = parse p ""

parseExpression :: Parser AST
parseExpression = try parseFullExpression <|> parseIntegerOnly

parseFullExpression :: Parser AST
parseFullExpression = do
  i1 <- parseInteger
  op <- parseOp
  i2 <- parseExpression
  return $ MkAST op i1 i2

parseInteger :: Parser AST
parseInteger = fmap Value (ignoreWhitespace $ fmap read (many1 digit))

parseIntegerOnly :: Parser AST
parseIntegerOnly = parseInteger >>= \i -> eof >> return i -- is there a simpler way to do this?

ignoreWhitespace :: Parser a -> Parser a
ignoreWhitespace = between spaces spaces

parseOp :: Parser Operator
parseOp = ignoreWhitespace $ fmap opToFunc (oneOf "+-*")

opToFunc :: Char -> Operator
opToFunc '+' = Add
opToFunc '-' = Subtract
opToFunc '*' = Multiply
