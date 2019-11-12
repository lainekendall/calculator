{-# LANGUAGE InstanceSigs #-}

module CalculatorPartTwo where

import Control.Applicative ((<|>))
import Data.Char
import Data.List
import qualified Data.Map as Map
import ParserModel
import ParserCombinators
import Evaluator

parseExpression :: Parser AST
parseExpression = parseFullValue <|> parseFullExpression

parseFullValue :: Parser AST
parseFullValue = parseValue <* parseEof

parseFullExpression :: Parser AST
parseFullExpression = do
  v <- parseValue
  op <- parseOperator
  fmap (MkAST op v) parseExpression

operatorsMap :: Map.Map Char Operator
operatorsMap = Map.fromList [('+', Add),
  ('-', Subtract),
  ('*', Multiply),
  ('/', Divide)]

parseValue :: Parser AST
parseValue = ignoreWhitespace $ fmap Value parseNumber

parseOperator :: Parser Operator
parseOperator = ignoreWhitespace $ unwrapMaybe $ fmap (`Map.lookup` operatorsMap) parseChar
