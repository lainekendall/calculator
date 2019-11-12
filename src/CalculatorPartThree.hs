module CalculatorPartThree where

import Evaluator
import Text.Parsec

type Parser = Parsec String ()

runParser :: Parsec String () a -> String -> Either ParseError a
runParser p = parse p ""

parseFullExpression :: Parser AST
parseFullExpression = undefined

parseExpression :: Parser Int
parseExpression = do
  spaces
  i1 <- parseInteger
  spaces
  op <- parseOp
  spaces
  i2 <- 
  spaces
  return $ op i1 i2

parseInteger :: Parser Int
parseInteger = fmap read (many digit)

parseOp :: Num a => Parser (a -> a -> a)
parseOp = fmap opToFunc (oneOf "+-*") 

opToFunc :: Num a => Char -> (a -> a -> a)
opToFunc '+' = (+)
opToFunc '-' = (-)
opToFunc '*' = (*)
