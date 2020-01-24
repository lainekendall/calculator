module CalcPartTwoSpec where

import CalculatorPartTwo
import Data.Char
import Data.Maybe (isNothing)
import Evaluator
import ParserModel
import ParserCombinators
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  describe "CalculatorPartTwo" $ do
    describe "parseOperator" $ do
      it "fails on empty" $ isNothing $ runParser parseOperator ""
      it "fails on other chars" $ isNothing $ runParser parseOperator "cc"
      it "Parses + and leaves the rest" $ runParser parseOperator "+123" == Just (Add, "123")
    describe "parseExpression" $ do
      it "fails on empty" $ isNothing $ runParser parseExpression ""
      it "fails on incomplete expressions" $ isNothing $ runParser parseExpression "1 + "
      it "fails on chars" $ isNothing $ runParser parseExpression "cc2 + c"
      it "parses values" $ runParser parseExpression "     1    " == Just (Value 1, "")
      it "parses simple expressions" $ runParser parseExpression "1 - 2" == Just (MkAST Subtract (Value 1) (Value 2),"")
      it "parses expressions" $ runParser parseExpression "     1    - 2 * 3+ 44 / 4" == Just (MkAST Subtract (Value 1) (MkAST Multiply (Value 2) (MkAST Add (Value 3) (MkAST Divide (Value 44) (Value 4)))),"")
