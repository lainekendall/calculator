module CalcPartThreeSpec where

import CalculatorPartThree
import Data.Maybe (isNothing)
import Evaluator
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  describe "CalculatorPartThree" $ do
    describe "parseExpression" $ do
      it "fails on empty" $ runParser parseExpression "12 + 34" == Right 46
