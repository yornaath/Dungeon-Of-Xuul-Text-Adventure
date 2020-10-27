module Game.Syntax.Parser where

import Prelude

import Control.Alt ((<|>))
import Data.Traversable (sequence, traverse_)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Game.Syntax.Spec (Expression(..), PlayerAction(..))
import Lib.Parser (Parser, anySpace, finiteString, runParser, someSpace, string)

actionParser :: Parser PlayerAction 
actionParser = move <|> take

move:: Parser PlayerAction
move = do
  _ <- sequence [anySpace, string "move", (string " to ") <|> someSpace]
  whereTo <- finiteString
  pure $ Move whereTo

take :: Parser PlayerAction
take = takeItemFrom <|> takeItem

takeItem :: Parser PlayerAction
takeItem = do
  _ <- sequence [anySpace, string "take", someSpace]
  itemName <- finiteString
  pure $ Take itemName

takeItemFrom:: Parser PlayerAction
takeItemFrom = do
  _ <- sequence [anySpace, string "take", someSpace]
  item <- finiteString
  _ <- sequence [anySpace, string "from", someSpace]
  from <- finiteString
  pure $ TakeItemFrom item from

expressionParser :: Parser Expression
expressionParser = load <|> save <|> turn

load :: Parser Expression
load = do
  _ <- anySpace
  _ <- string "load"
  _ <- someSpace
  saveGame <- finiteString
  pure (Load saveGame)

save :: Parser Expression
save = do
  _ <- anySpace
  _ <- string "save"
  _ <- someSpace
  saveGame <- finiteString
  pure (Load saveGame)

turn :: Parser Expression
turn = do 
  action <- actionParser
  pure $ Turn action

main :: Effect Unit
main = do
  logShow $ runParser (expressionParser) "move to north"
    --logShow $ runParser (sequence [string "foo", string "bar"]) "foobar"