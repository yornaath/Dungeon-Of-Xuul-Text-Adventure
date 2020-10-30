module Test.Main where

import Prelude

import Effect (Effect)
import Test.Game.Syntax.Parser (gameParserTests)
import Test.Game.Data.Character.Inventory (inventoryTests)
import Test.Lib.Parser (parserTests)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  parserTests
  gameParserTests
  inventoryTests