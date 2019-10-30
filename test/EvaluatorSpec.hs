module EvaluatorSpec where

import qualified Evaluator as E
import Test.Hspec

spec :: Spec
spec =
  describe "evaluate" $ do
    it "errors on Nothing" $ E.evaluate Nothing == "Error!"
    it "errors when it can't finish" $
      E.evaluate (Just (E.Value 1, "the rest")) ==
      "Didn't finish parsing. So far: E.Value 1. Still left: the rest"
    it "correctly E.evaluates expressions" $
      E.evaluate
        (Just
           ( E.MkAST
               E.Add
               (E.Value 1)
               (E.MkAST
                  E.Subtract
                  (E.Value 2)
                  (E.MkAST
                     E.Multiply
                     (E.Value 4)
                     (E.MkAST E.Divide (E.Value 5) (E.Value 10))))
           , "")) ==
      "3"
