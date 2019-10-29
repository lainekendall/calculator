{-# LANGUAGE InstanceSigs #-}

module CalculatorPartOne where

import Data.Char
import Data.List

runParse :: Show a => String -> Parser a -> Maybe a
runParse xs p = fmap (\(a, _) -> a) $ (runParser p) xs

readUserInput :: IO String
readUserInput = undefined

parseExpression :: Parser AST
parseExpression = do
  i1 <- parseValue 
  parseSpace
  op <- parseOperator
  parseSpace
  i2 <- parseValue
  return $ op i1 i2

parseChar :: Parser Char
parseChar = Parser f
  where
    f "" = Nothing
    f (c:cs) = Just (c, cs)

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

eval :: AST -> Integer
eval (Value i) = i
eval (Add a b) = eval a + eval b
eval (Subtract a b) = eval a - eval b
eval (Multiply a b) = eval a * eval b
eval (Divide a b) = eval a `div` eval b

data AST = Value Integer |  Add AST AST | Subtract AST AST | Multiply AST AST | Divide AST AST deriving Show
-------------------------
--- Parsing Machinery ---
-------------------------
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \s -> case p s of
                      Nothing -> Nothing
                      Just (a, s1) -> Just (f a, s1)

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \s -> Just (a, s)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) (Parser pf) pa = Parser $ \s -> case pf s of
                        Nothing -> Nothing
                        Just (f, s1) -> runParser (fmap f pa) s1

instance Monad Parser where
  (>>=)  :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) (Parser pa) aToPb = Parser $ \s -> case pa s of
    Nothing -> Nothing
    Just (a, s1) -> runParser (aToPb a) s1
