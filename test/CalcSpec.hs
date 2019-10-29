module CalcPartOneSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Char 
import CalculatorPartOne

spec :: Spec
spec = do
  describe "Calculator" $ do
    describe "isSpace" $ do
      it "Empty" $
       runParser parseSpace "" == Nothing
      it "space" $
       runParser parseSpace " " == Just ((), "")
      it "\n" $
       runParser parseSpace "\n" == Just ((), "")
      it "space then words" $
       runParser parseSpace "          3 + 4    " == Just ((), "3 + 4    ")
    describe "parseDigit" $ do
      it "Empty" $ do
       runParser parseDigit "" == Nothing
      it "space " $ do
       runParser parseDigit " " == Nothing
      it "word" $ do
       runParser parseDigit "hello" == Nothing
      it "345  " $ do
       runParser parseDigit "345  " == Just (3, "45  ")
    describe "parseInteger" $ do
      it "Empty" $ do
       runParser parseInteger "" == Nothing
      it "space " $ do
       runParser parseInteger " " == Nothing
      it "word" $ do
       runParser parseInteger "hello" == Nothing
      it "345  " $ do
       runParser parseInteger "345  " == Just (3, "45  ")
