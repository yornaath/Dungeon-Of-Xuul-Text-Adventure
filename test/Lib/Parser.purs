module Test.Lib.Parser where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Lib.Parser (anySpace, finiteString, literal, runParser, someSpace)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert

parserTests :: Free TestF Unit
parserTests = do
  suite "Lib.Parser" do
    suite "runParser" do

      suite "finiteString" do
        test "general" do
          Assert.equal (Right (Tuple "foo" ""))
            $ runParser finiteString "foo"
        test "accepts symbols and digits" do
          Assert.equal (Right (Tuple "123*ˆ?" ""))
            $ runParser finiteString "123*ˆ?"

      suite "someSpace" do  
        test "general" do
          Assert.equal (Right (Tuple " " "foo"))
            $ runParser someSpace " foo"
          Assert.equal (Right (Tuple "  " "foo"))
            $ runParser someSpace "  foo"

      suite "anySpace" do
        test "general" do
          Assert.equal (Right (Tuple "" "foo"))
            $ runParser anySpace "foo"
          Assert.equal (Right (Tuple "  " "foo"))
            $ runParser anySpace "  foo"
      
      suite "literal" do  
        test "general" do
          Assert.equal (Right (Tuple "something" ""))
            $ runParser (literal "something") "something"