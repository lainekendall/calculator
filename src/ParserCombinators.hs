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

parseSpaces :: Parser String
parseSpaces = many $ satisfy isSpace

parseEof :: Parser ()
parseEof = Parser f
  where
    f "" = Just ((), "")
    f s = Nothing

parseSpace :: Parser ()
parseSpace = Parser f
      where
        f s = Just ((), dropWhile isSpace s)

ignoreWhitespace :: Parser a -> Parser a
ignoreWhitespace = between parseSpaces parseSpaces

unwrapMaybe :: Parser (Maybe a) -> Parser a
unwrapMaybe p =
  p >>= \case
    Nothing -> Parser $ const Nothing
    Just op -> return op

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

between :: Parser a -> Parser a -> Parser b -> Parser b
between pOpen pClose p = pOpen >>= \x -> p >>= \y -> pClose >>= \z -> return y

oneOf :: String -> Parser Char
oneOf "" = Parser $ const Nothing
oneOf (c:cs) = choice (char c : [oneOf cs])
