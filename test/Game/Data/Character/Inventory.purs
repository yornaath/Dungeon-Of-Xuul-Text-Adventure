module Test.Game.Data.Character.Inventory where

import Prelude

import Control.Monad.Free (Free)
import Test.Unit (TestF, test, suite)
import Test.Unit.Assert as Assert

inventoryTests :: Free TestF Unit
inventoryTests = do
  suite "Game.Data.Character.Inventory" do
    suite "general" do
      test "test" do
        Assert.equal true true