module Test.Game.Syntax.Parser where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Game.Syntax.Parser (move)
import Game.Syntax.Spec (PlayerAction(..))
import Lib.Parser (finiteString, runParser)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert

a = runParser move "move north"

gameParserTests :: Free TestF Unit
gameParserTests = do
  suite "Game.Syntax.Parser" do
    suite "general" do

      test "move" do
        Assert.equal (Right $ Tuple (Move "north") "")
          $ runParser move "move north"
        Assert.equal (Right $ Tuple (Move "west") "")
          $ runParser move "move to west"