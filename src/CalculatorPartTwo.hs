{-# LANGUAGE InstanceSigs #-}

module CalculatorPartTwo where

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

parsery :: Parser (Char, Char)
parsery = parseChar >>= \c -> parseChar >>= \c2 -> pure (c, c2)

many :: Parser a -> Parser [a]
many f = some f <|> (return [])

some :: Parser a -> Parser [a]
some f = do
  x <- f
  xs <- many f
  return $ x : xs

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = parseChar >>= \c -> if f c then return c else Parser $ \s -> Nothing

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) (Parser p) (Parser p1) = Parser $ \s -> case p s of
  Nothing -> p1 s
  x -> x

combine :: Parser a -> Parser a -> Parser [a]
combine p p1 = p >>= \a -> p1 >>= \a1 -> pure $ a : [a1]

parseChar :: Parser Char
parseChar = Parser f
  where
    f "" = Nothing
    f (c:cs) = Just (c, cs)

parseNumber' :: Parser Integer
parseNumber' = fmap read $ many $ satisfy isDigit

parseNumber :: Parser Integer
parseNumber = Parser f
  where
    f "" = Nothing
    f s@(c:cs) = if isDigit c 
                 then Just (read $ takeWhile isDigit s, dropWhile isDigit s)
                 else Nothing

parseValue :: Parser AST
parseValue = fmap Value parseNumber

parseSpace' :: Parser [Char] -- ??
parseSpace' = some $ satisfy isSpace

parseSpace :: Parser ()
parseSpace = Parser f
  where 
    f "" = Nothing
    f s = Just ((), dropWhile isSpace s)

char :: Char -> Parser Char
char c = undefined

string :: String -> Parser String
string xs = undefined -- comes from char

parseOperator :: Parser (AST -> AST -> AST)
parseOperator = Parser f
  where
    f "" = Nothing
    f (c:cs) = case c of
                 '+' -> Just ( Add, cs)
                 '-' -> Just ( Subtract, cs)
                 '*' -> Just ( Multiply, cs)
                 '/' -> Just ( Divide, cs)

--parseValue :: Parser AST
--parseValue = undefined --mapP Value parseInt
--
--eval :: AST -> Integer
--eval (Value i) = i
--eval (Add a b) = eval a + eval b
--eval (Subtract a b) = eval a - eval b
--eval (Multiply a b) = eval a * eval b
--eval (Divide a b) = eval a `div` eval b
--
--prettyPrint :: AST -> String
--prettyPrint = undefined
--
data AST = Value Integer |  Add AST AST | Subtract AST AST | Multiply AST AST | Divide AST AST deriving Show
--
--testAST = Value 3
--testAST1 = Add (Value 1) (Value 2)
--
--testInput = "3"
--testInput1 = "1 + 2"
--testInput2 = "1 - 3 + 4"
--
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
 
fmpp :: Monad m => (a -> b) -> m a -> m b
fmpp f ma = ma >>= \a -> return (f a)

app :: Monad m => m (a -> b) -> m a -> m b
app mf ma = mf >>= \f -> ma >>= \a -> return (f a)
parsery' :: Parser String
parsery' = do
  c <- parseChar
  c2 <- parseChar
  pure [c,c2]
