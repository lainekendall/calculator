module Evaluator where

evaluate :: Maybe (AST, String) -> Integer
evaluate Nothing = error "Error!"
evaluate (Just (a, "")) = eval a
evaluate (Just (a, s1)) = error $ "Didn't finish parsing. So far: " ++ show a ++ ". Still left: " ++ s1 

eval :: AST -> Integer
eval (Value i) = i
eval (Add a b) = eval a + eval b
eval (Subtract a b) = eval a - eval b
eval (Multiply a b) = eval a * eval b
eval (Divide a b) = eval a `div` eval b

data AST = Value Integer |  Add AST AST | Subtract AST AST | Multiply AST AST | Divide AST AST deriving Show
