module CalcPartThreeSpec where

import CalculatorPartThree
import Data.Either (isLeft)
import Data.Maybe (isNothing)
import Evaluator
import Test.Hspec
import Test.QuickCheck
import ParserModel

spec :: Spec
spec =
  describe "CalculatorPartThree" $ do
    describe "parseValue" $ do
      it "Empty" $ isNothing $ runParser parseValue ""
      it "space " $ isNothing $ runParser parseValue " "
      it "word" $ isNothing $ runParser parseValue "hello"
      it "345  " $ runParser parseValue "345  " == Just (Value 345, "")
      it "  345  " $ runParser parseValue "  345  " == Just (Value 345,"")
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
      it "parses a value of an incomplete expr" $ isNothing $ runParser parseExpression "1 +   "
      it "trims all excess white space" $
        runParser parseExpression "   1   -  2    " ==
        Just (MkAST Subtract (Value 1) (Value 2), "")
      it "parses multiple expressions" $
        runParser parseExpression "1 + 2 * 10 - 7" ==
        Just
          ( MkAST
              Add
              (Value 1)
              (MkAST Multiply (Value 2) (MkAST Subtract (Value 10) (Value 7)))
          , "")
