module EvaluatorSpec where

import Test.Hspec
import Evaluator

spec :: Spec
spec = do
  describe "eval" $ do
    it "errors on Nothing" $ do
      evaluate Nothing == "Error!"
    it "errors when it can't finish" $ do
      (evaluate $ Just (Value 1, "the rest")) == "Didn't finish parsing. So far: Value 1. Still left: the rest"
    it "correctly evaluates expressions" $ do
      (evaluate $ Just (MkAST Add (Value 1) (MkAST Subtract (Value 2) (MkAST Multiply (Value 4) (MkAST Divide (Value 5) (Value 10)))),"")) == "3"
