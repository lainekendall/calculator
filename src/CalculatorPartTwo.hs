{-# LANGUAGE InstanceSigs #-}

module CalculatorPartTwo where

import Data.Char
import Data.List
import ParserModel
import Evaluator

--runParse :: Show a => String -> Parser a -> Maybe a
--runParse xs p = fmap (\(a, _) -> a) $ (runParser p) xs
--readUserInput :: IO String
--readUserInput = undefined

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

parseChar :: Parser Char
parseChar = Parser f
  where
    f "" = Nothing
    f (c:cs) = Just (c, cs)

parseNumber :: Parser Integer
parseNumber = fmap read $ many $ satisfy isDigit

parseValue :: Parser AST
parseValue = fmap Value parseNumber

parseSpace :: Parser String
parseSpace = many $ satisfy isSpace

char :: Char -> Parser Char
char c = satisfy (==c)

string :: String -> Parser String
string "" = return ""
string (c:cs) = do
  c1 <- char c
  cs1 <- string cs
  return $ c1 : cs1

--parseOperator :: Parser (AST -> AST -> AST)
--parseOperator = Parser f
--  where
--    f "" = Nothing
--    f (c:cs) = case c of
--                 '+' -> Just ( Add, cs)
--                 '-' -> Just ( Subtract, cs)
--                 '*' -> Just ( Multiply, cs)
--                 '/' -> Just ( Divide, cs)
--
----parseValue :: Parser AST
----parseValue = undefined --mapP Value parseInt
----
----eval :: AST -> Integer
----eval (Value i) = i
----eval (Add a b) = eval a + eval b
----eval (Subtract a b) = eval a - eval b
----eval (Multiply a b) = eval a * eval b
----eval (Divide a b) = eval a `div` eval b
----
----prettyPrint :: AST -> String
----prettyPrint = undefined
----
--data AST = Value Integer |  Add AST AST | Subtract AST AST | Multiply AST AST | Divide AST AST deriving Show
----
----testAST = Value 3
----testAST1 = Add (Value 1) (Value 2)
----
----testInput = "3"
----testInput1 = "1 + 2"
----testInput2 = "1 - 3 + 4"
----
---------------------------
----- Parsing Machinery ---
---------------------------
--newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }
--
--instance Functor Parser where
--  fmap :: (a -> b) -> Parser a -> Parser b
--  fmap f (Parser p) = Parser $ \s -> case p s of
--                      Nothing -> Nothing
--                      Just (a, s1) -> Just (f a, s1)
--
--instance Applicative Parser where
--  pure :: a -> Parser a
--  pure a = Parser $ \s -> Just (a, s)
--  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
--  (<*>) (Parser pf) pa = Parser $ \s -> case pf s of
--                        Nothing -> Nothing
--                        Just (f, s1) -> runParser (fmap f pa) s1
--
--instance Monad Parser where
--  (>>=)  :: Parser a -> (a -> Parser b) -> Parser b
--  (>>=) (Parser pa) aToPb = Parser $ \s -> case pa s of
--    Nothing -> Nothing
--    Just (a, s1) -> runParser (aToPb a) s1
-- 
--fmpp :: Monad m => (a -> b) -> m a -> m b
--fmpp f ma = ma >>= \a -> return (f a)
--
--app :: Monad m => m (a -> b) -> m a -> m b
--app mf ma = mf >>= \f -> ma >>= \a -> return (f a)
--parsery' :: Parser String
--parsery' = do
--  c <- parseChar
--  c2 <- parseChar
--  pure [c,c2]
