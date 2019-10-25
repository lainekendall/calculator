module Calculator where

import Data.Char
import Data.List

runParse :: Show a => String -> Parser a -> Maybe a
runParse xs p = fmap (\(a, _) -> a) $ (runParser p) xs


--readUserInput :: IO String
--readUserInput = undefined
--
--parse :: Parser AST
--parse "" = Nothing
--parse s = undefined
--
--parseInt :: Parser Integer
--parseInt [] = Nothing
--parseInt (c:cs) | isDigit c = Just (read [c], cs)
--                | otherwise = Nothing
--
--parseSpace :: Parser ()
--parseSpace [] = Nothing
--parseSpace s = Just ((), dropWhile isSpace s)
--
--parseOperator :: Char -> (AST -> AST -> AST)
--parseOperator '+' = Add
--parseOperator '-' = Subtract
--parseOperator '*' = Multiply
--parseOperator '/' = Divide
--
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
--data AST = Value Integer |  Add AST AST | Subtract AST AST | Multiply AST AST | Divide AST AST deriving Show
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
  fmap = mapP

fmpp :: Monad m => (a -> b) -> m a -> m b
fmpp f xs = xs >>= \x -> return (f x)

app :: Monad m => m (a -> b) -> m a -> m b
app mf ma = mf >>= \f -> ma >>= \a -> return (f a)
  
instance Applicative Parser where
  pure = pureP
  (<*>) = appP
instance Monad Parser where
  (>>=) = bindP

mapP :: (a -> b) -> Parser a -> Parser b
mapP f (Parser p) = Parser $ \s -> case p s of
                      Nothing -> Nothing
                      Just (a, s1) -> Just (f a, s1)

appP :: Parser (a -> b) -> Parser a -> Parser b
appP (Parser pf) pa = Parser $ \s -> case pf s of
                        Nothing -> Nothing
                        Just (f, s1) -> runParser (mapP f pa) s1

----appP pf pa = \s -> case pa s of
----                        Nothing -> Nothing
----                        Just (a, s1) -> case pf s1 of
----                                             Nothing -> Nothing
----                                             Just (f, s2) -> Just (f a, s2)
--
pureP :: a -> Parser a
pureP a = Parser $ \s -> Just (a, s)

bindP :: Parser a -> (a -> Parser b) -> Parser b
bindP (Parser pa) aToPb = Parser $ \s -> case pa s of
  Nothing -> Nothing
  Just (a, s1) -> runParser (aToPb a) s1

parserx :: Parser Char
parserx = undefined

parsery :: Parser (Char, Char)
parsery = parserx `bindP` \c -> parserx `bindP` \c2 -> pureP (c, c2)

parsery' :: Parser String
parsery' = do
  c <- parserx
  c2 <- parserx
  pure [c,c2]




---- bindP pa apb s = undefined -- how do I write this in terms of map??? (or app...?)
--
--pureM :: a -> Maybe a
--pureM = Just
--
--mapMaybe :: (a -> b) -> Maybe a -> Maybe b
--mapMaybe _ Nothing = Nothing              
--mapMaybe f (Just a) = Just $ f a          
--
--appM:: Maybe (a -> b) -> Maybe a -> Maybe b
--appM Nothing = \ma -> Nothing
--appM (Just f) = mapMaybe f
--
--bindM :: Maybe a -> (a -> Maybe b) -> Maybe b
--bindM Nothing _ = Nothing
--bindM (Just a) f = f a
----bindM = undefined -- how do I write this in terms of map??? (or app...?)
--
---- you probably wont use this one:
--smushParsers :: Semigroup a => Parser a -> Parser a -> Parser a
--smushParsers p p1 = \x -> 
--  case p x of
--    Nothing -> Nothing
--    Just (a, rest) -> 
--      case p1 rest of
--        Nothing -> Nothing
--        Just (a1, rest1) -> Just (a <> a1, rest1)
--
