module CalcPartOneSpec where

import CalculatorPartOne
import Data.Char
import Data.Maybe (isNothing)
import Evaluator
import ParserModel
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  describe "CalculatorPartOne" $ do
    describe "parseSpace" $ do
      it "Empty" $ runParser parseSpace "" == Just ((), "")
      it "space" $ runParser parseSpace " " == Just ((), "")
      it "\n" $ runParser parseSpace "\n" == Just ((), "")
      it "space then words" $
        runParser parseSpace "          3 + 4    " == Just ((), "3 + 4    ")
    describe "parseValue" $ do
      it "Empty" $ isNothing $ runParser parseValue ""
      it "space " $ isNothing $ runParser parseValue " "
      it "word" $ isNothing $ runParser parseValue "hello"
      it "345  " $ runParser parseValue "345  " == Just (Value 345, "  ")
      it "  345  " $ isNothing $ runParser parseValue "  345  "
    describe "parseOperator" $ do
      it "Empty" $ isNothing $ runParser parseOperator ""
      it "space " $ isNothing $ runParser parseOperator " "
      it "word" $ isNothing $ runParser parseOperator "hello"
      it "number" $ isNothing $ runParser parseOperator "345"
      it "+" $ runParser parseOperator "+" == Just (Add, "")
    describe "parseFullExpression" $ do
      it "Empty" $ isNothing $ runParser parseFullExpression ""
      it "space" $ isNothing $ runParser parseFullExpression " "
      it "incomplete" $ isNothing $ runParser parseFullExpression "1 +   "
      it "cannot parse a single value" $
        isNothing $ runParser parseFullExpression "1"
    describe "parseExpression" $ do
      it "Empty" $ isNothing $ runParser parseExpression ""
      it "space" $ isNothing $ runParser parseExpression " "
      it "parses a value of an incomplete expr" $
        runParser parseExpression "1 +   " == Just (Value 1, " +   ")
      it "trims all excess white space" $
        runParser parseExpression "   1   -  2    " ==
        Just (MkAST Subtract (Value 1) (Value 2), "")
      it "parses multiple expressions" $
        runParser parseExpression "1 + 2 * 10 / 7" ==
        Just
          ( MkAST
              Add
              (Value 1)
              (MkAST Multiply (Value 2) (MkAST Divide (Value 10) (Value 7)))
          , "")
