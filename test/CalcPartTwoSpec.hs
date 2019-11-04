module CalcPartTwoSpec where

import CalculatorPartTwo
import Data.Char
import Data.Maybe (isNothing)
import Evaluator
import ParserModel
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  describe "CalculatorPartOne" $ do
    describe "parseChar" $ do
      it "Empty" $ do 
        isNothing $ runParser parseChar ""
      it "parses a char correctly" $ runParser parseChar "c" == Just ('c', "")
      it "only parses one char and leaves the rest" $ runParser parseChar "cab" == Just ('c', "ab")
    describe "parseNumber" $ do
      it "Empty" $ isNothing $ runParser parseNumber ""
      it "parses a number" $ runParser parseNumber "123" == Just (123, "")
    describe "parseValue" $ do
      it "Empty" $ isNothing $ runParser parseValue ""
      it "parses a number into a AST Value" $ runParser parseValue "123" == Just (Value 123, "")
    describe "parseSpace" $ do
      it "Won't fail on empty" $ runParser parseSpace "" == Just ("", "")
      it "Doesn't parse numbers" $ runParser parseSpace "123" == Just ("", "123")
      it "Parses multiple spaces" $ runParser parseSpace "       123  " == Just ("", "123  ")
    describe "string" $ do
      -- Add quick check for when s == s
      it "Empty" $ isNothing $ runParser (string "") ""
      it "Parses strings" $ runParser (string "Hello") "Hello! hi" == Just ("Hell", "! hi  h")
