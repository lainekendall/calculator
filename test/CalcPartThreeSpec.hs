module CalcPartThreeSpec where

import CalculatorPartThree
import Data.Maybe (isNothing)
import Data.Either (isLeft)
import Evaluator
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  describe "CalculatorPartThree" $ do
    describe "parseIntegerOnly" $ do
      it "fails on empty" $ isLeft $ runParser parseIntegerOnly "    12 +"
      it "fails on chars" $ isLeft $ runParser parseIntegerOnly "c12"
      it "fails on spaces" $ isLeft $ runParser parseIntegerOnly "  "
      it "parses an integer with whitespace" $ runParser parseIntegerOnly "    12      " == Right (Value 12)
      it "parses an integer" $ runParser parseIntegerOnly "12" == Right (Value 12)
    describe "parseInteger" $ do
      it "fails on empty" $ isLeft $ runParser parseInteger ""
      it "fails on chars" $ isLeft $ runParser parseInteger "c12"
      it "fails on spaces" $ isLeft $ runParser parseInteger "  "
      it "parses an integer" $ runParser parseInteger "12" == Right (Value 12)
      it "only parses an integer" $ runParser parseInteger "12 + 34" == Right (Value 12)
      it "ignores everything after the integer" $ runParser parseInteger "  12xxc  " == Right (Value 12)
      it "ignores whitespace" $ runParser parseInteger "  12  " == Right (Value 12)
    describe "parseOp" $ do
      it "fails on empty" $ isLeft $ runParser parseOp ""
      it "fails on chars" $ isLeft $ runParser parseOp "c12"
      it "fails on spaces" $ isLeft $ runParser parseOp "  "
      it "fails if anything before" $ isLeft $ runParser parseOp "  12 - 4 "
      it "parses +" $ runParser parseOp "   + " == Right Add
      it "parses -" $ runParser parseOp "   -  " == Right Subtract
      it "parses *" $ runParser parseOp "   *   " == Right Multiply
      it "ignores everything after the operator" $ runParser parseOp "-12xxc  " == Right Subtract
    describe "parseExpression" $ do
      it "fails on empty" $ isLeft $ runParser parseExpression ""
      it "fails on chars" $ isLeft $ runParser parseExpression "c12  "
      it "fails on spaces" $ isLeft $ runParser parseExpression "  "
      it "fails on ops" $ isLeft $ runParser parseExpression "   -   "
      it "parses an integer" $ runParser parseExpression "12" == Right (Value 12)
      it "parses a simple expression" $ runParser parseExpression "12 + 10" == Right (MkAST Add (Value 12) (Value 10))
      it "parses a complex expression" $ runParser parseExpression "   12 + 10   *55-10   +2  " == Right (MkAST Add (Value 12) (MkAST Multiply (Value 10) (MkAST Subtract (Value 55) (MkAST Add (Value 10) (Value 2)))))
