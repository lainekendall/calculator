{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module CalculatorPartTwo where

import Data.Char
import Data.List
import qualified Data.Map as Map
import ParserModel
import Evaluator

parseExpression :: Parser AST
parseExpression = undefined

operatorsMap :: Map.Map Char Operator
operatorsMap = Map.fromList [('+', Add),
  ('-', Subtract),
  ('*', Multiply),
  ('/', Divide)]

-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Applicative.html#t:Alternative
many :: Parser a -> Parser [a]
many f = some f <|> return []

some :: Parser a -> Parser [a]
some f = do
  x <- f
  xs <- many f
  return $ x : xs

-- http://dev.stephendiehl.com/fun/002_parsers.html
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Text-ParserCombinators-ReadP.html#v:satisfy

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = parseChar >>= \c -> if f c
                                then return c
                                else Parser $ const Nothing

char :: Char -> Parser Char
char c = satisfy (==c)

string :: String -> Parser String
string "" = return ""
string (c:cs) = do
  c1 <- char c
  cs1 <- string cs
  return $ c1 : cs1

parseChar :: Parser Char
parseChar = Parser f
  where
    f "" = Nothing
    f (c:cs) = Just (c, cs)

parseNumber :: Parser Integer
parseNumber = fmap read $ some $ satisfy isDigit

parseValue :: Parser AST
parseValue = fmap Value parseNumber

parseSpace :: Parser String
parseSpace = many $ satisfy isSpace

parseMaybeOperator :: Parser (Maybe Operator)
parseMaybeOperator = fmap (`Map.lookup` operatorsMap) parseChar

parseOperator :: Parser Operator
parseOperator = parseMaybeOperator >>= \case
                                   Nothing -> Parser $ const Nothing
                                   Just op -> return op
choice :: [Parser a] -> Parser a
choice [pa] = pa
choice (pa:pas) = pa <|> choice pas
