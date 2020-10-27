module Test.Game.Syntax.Parser where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Game.Syntax.Parser (move, take, turn)
import Game.Syntax.Spec (Expression(..), PlayerAction(..))
import Lib.Parser (runParser)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert


gameParserTests :: Free TestF Unit
gameParserTests = do
  suite "Game.Syntax.Parser" do
    suite "general" do

      test "move" do
        Assert.equal (Right $ Tuple (Move "north") "")
          $ runParser move "move north"
        Assert.equal (Right $ Tuple (Move "west") "")
          $ runParser move "move to west"
      
      test "take" do
        Assert.equal (Right $ Tuple (Take "dagger") "")
          $ runParser take "take dagger"

      test "take from" do
        Assert.equal (Right $ Tuple (TakeItemFrom "cookie" "basket") "")
          $ runParser take "take cookie from basket"
      
      test "turn" do
        Assert.equal (Right $ Tuple (Action $ Move "north") "")
          $ runParser turn "move north"