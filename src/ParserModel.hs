{-# LANGUAGE InstanceSigs #-}

module ParserModel where

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

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) (Parser p) (Parser p1) =
  Parser $ \s -> case p s of
    Nothing -> p1 s
    x -> x
