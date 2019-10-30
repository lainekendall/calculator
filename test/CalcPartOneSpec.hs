module CalcPartOneSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Char 
import CalculatorPartOne
import ParserModel
import Evaluator

spec :: Spec
spec = do
  describe "CalculatorPartOne" $ do
    describe "parseSpace" $ do
      it "Empty" $
       runParser parseSpace "" == Just ((), "")
      it "space" $
       runParser parseSpace " " == Just ((), "")
      it "\n" $
       runParser parseSpace "\n" == Just ((), "")
      it "space then words" $
       runParser parseSpace "          3 + 4    " == Just ((), "3 + 4    ")
    describe "parseValue" $ do
      it "Empty" $ do
       runParser parseValue "" == Nothing
      it "space " $ do
       runParser parseValue " " == Nothing
      it "word" $ do
       runParser parseValue "hello" == Nothing
      it "345  " $ do
       runParser parseValue "345  " == Just (Value 345, "  ")
      it "  345  " $ do
       runParser parseValue "  345  " == Nothing
    describe "parseOperator" $ do
      it "Empty" $ do
       runParser parseOperator "" == Nothing
      it "space " $ do
       runParser parseOperator " " == Nothing
      it "word" $ do
       runParser parseOperator "hello" == Nothing
      it "number" $ do
       runParser parseOperator "345" == Nothing
      it "+" $ do
       runParser parseOperator "+" == Just (Add, "")
    describe "parseFullExpression" $ do
      it "Empty" $ do
       runParser parseFullExpression "" == Nothing
      it "space" $ do
       runParser parseFullExpression " " == Nothing
      it "incomplete" $ do
       runParser parseFullExpression "1 +   " == Nothing
      it "parses an expression but doesn't trim spaces off end" $ do
       runParser parseFullExpression "1 -  2    " == Just (MkAST Subtract ( Value 1) (Value 2), "")
      it "cannot parse multiple expressions" $ do
       runParser parseFullExpression "1   + 2 - 4" == Just (MkAST Add ( Value 1) (Value 2), "- 4")
      it "fails with leading whitespace" $ do
       runParser parseFullExpression "   1 * 2 - 4" == Just (MkAST Multiply ( Value 1) (Value 2), "- 4")
    describe "parseExpression" $ do
      it "Empty" $ do
       runParser parseExpression "" == Nothing
      it "space" $ do
       runParser parseExpression " " == Nothing
      it "incomplete" $ do
       runParser parseExpression "1 +   " == Just (Value 1, " +   ")
      it "parses an expression but doesn't trim spaces off end" $ do
       runParser parseExpression "1 -  2    " == Just (MkAST Subtract ( Value 1) (Value 2), "")
      it "cannot parse multiple expressions" $ do
       runParser parseExpression "1   + 2 - 4" == Just (MkAST Add ( Value 1) (Value 2), "- 4")
      it "fails with leading whitespace" $ do
       runParser parseExpression "   1 * 2 - 4" == Just (MkAST Multiply ( Value 1) (Value 2), "- 4")

