{-# LANGUAGE LambdaCase #-}

module ParserCombinators where

import Control.Applicative ((<|>))
import Data.Char
import ParserModel

-- Parsers
parseChar :: Parser Char
parseChar = Parser f
  where
    f "" = Nothing
    f (c:cs) = Just (c, cs)

parseNumber :: Parser Integer
parseNumber = fmap read $ some $ satisfy isDigit

parseSpace :: Parser String
parseSpace = many $ satisfy isSpace

-- Parser Combinators
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Applicative.html#t:Alternative
many :: Parser a -> Parser [a]
many f = some f <|> return []

some :: Parser a -> Parser [a]
some f = do
  x <- f
  xs <- many f
  return $ x : xs

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string "" = return ""
string (c:cs) = do
  c1 <- char c
  cs1 <- string cs
  return $ c1 : cs1

-- http://dev.stephendiehl.com/fun/002_parsers.html
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Text-ParserCombinators-ReadP.html#v:satisfy
satisfy :: (Char -> Bool) -> Parser Char
satisfy f =
  parseChar >>= \c ->
    if f c
      then return c
      else Parser $ const Nothing

choice :: [Parser a] -> Parser a
choice [pa] = pa
choice (pa:pas) = pa <|> choice pas

unwrapMaybe :: Parser (Maybe a) -> Parser a
unwrapMaybe p =
  p >>= \case
    Nothing -> Parser $ const Nothing
    Just op -> return op

ignoreWhitespace :: Parser a -> Parser a
ignoreWhitespace p = parseSpace >> p >>= \v -> parseSpace >> return v

parseEof :: Parser ()
parseEof = Parser f
  where
    f "" = Just ((), "")
    f s = Nothing
