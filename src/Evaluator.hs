{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Evaluator where

import ParserModel

evaluate :: Maybe (AST, String) -> String
evaluate Nothing = "Error!"
evaluate (Just (a, "")) = show . eval $ a
evaluate (Just (a, s1)) = "Didn't finish parsing. So far: " ++ show a ++ ". Still left: " ++ s1 

eval :: AST -> Integer
eval (Value i) = i
eval (MkAST Add a b) = eval a + eval b
eval (MkAST Subtract a b) = eval a - eval b
eval (MkAST Multiply a b) = eval a * eval b
eval (MkAST Divide a b) = eval a `div` eval b

data AST = Value Integer | MkAST Operator AST AST deriving (Show, Eq)

data Operator = Add | Subtract | Multiply | Divide deriving (Show, Eq)
