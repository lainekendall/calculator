module ParserCombinatorsSpec where
import Test.Hspec
import Test.QuickCheck
import ParserCombinators
import ParserModel
import Evaluator
import Data.Maybe (isNothing)


spec :: Spec
spec =
  describe "Parsercombinators" $
    describe "parseChar" $ do
  it "fails on empty" $
    isNothing $ runParser parseChar ""
  it "parses a char correctly" $ runParser parseChar "c" == Just ('c', "")
  it "only parses one char and leaves the rest" $ runParser parseChar "cab" == Just ('c', "ab")
    -- describe "parseSpace" $ do
    --   it "Won't fail on empty" $ runParser parseSpace "" == Just ("", "")
    --   it "Doesn't parse numbers" $ runParser parseSpace "123" == Just ("", "123")
    --   it "Parses multiple spaces" $ runParser parseSpace "       123  " == Just ("       ","123  ")
    -- describe "string" $ do
    --   -- Add quick check for when s == s
    --   it "won't fail on empty" $ runParser (string "") "" == Just ("", "")
    --   it "Parses strings" $ runParser (string "Hello") "Hello! hi" == Just ("Hello","! hi")
