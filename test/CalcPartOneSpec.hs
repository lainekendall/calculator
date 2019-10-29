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
       runParser parseSpace "" == Nothing
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
