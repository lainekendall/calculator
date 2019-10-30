module CalcPartTwoSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Char
import CalculatorPartTwo
import ParserModel
import Evaluator

spec :: Spec
spec =
  describe "CalculatorPartOne" $ do
  describe "parseSpace" $ do
    it "Empty" $
     True == True
