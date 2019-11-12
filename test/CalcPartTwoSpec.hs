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
    describe "parseChar" $ do
      it "fails on empty" $
        isNothing $ runParser parseChar ""
      it "parses a char correctly" $ runParser parseChar "c" == Just ('c', "")
      it "only parses one char and leaves the rest" $ runParser parseChar "cab" == Just ('c', "ab")
    describe "parseValue" $ do
      it "fails on empty" $ isNothing $ runParser parseValue ""
      it "can't parse chars" $ isNothing $ runParser parseValue "cc"
      it "parses a number into a AST Value" $ runParser parseValue "123cc1" == Just (Value 123,"cc1")
    describe "parseSpace" $ do
      it "Won't fail on empty" $ runParser parseSpace "" == Just ("", "")
      it "Doesn't parse numbers" $ runParser parseSpace "123" == Just ("", "123")
      it "Parses multiple spaces" $ runParser parseSpace "       123  " == Just ("       ","123  ")
    describe "string" $ do
      -- Add quick check for when s == s
      it "won't fail on empty" $ runParser (string "") "" == Just ("", "")
      it "Parses strings" $ runParser (string "Hello") "Hello! hi" == Just ("Hello","! hi")
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
