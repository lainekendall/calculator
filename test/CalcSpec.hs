module CalcSpec where

import Test.Hspec
import calculator.Main

main :: IO ()
main = hspec $ do
  describe "Calculator" $ do
    it "can add" $
     runParse "1 + 2" undefined `shouldBe` 3 
