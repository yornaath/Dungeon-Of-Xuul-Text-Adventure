module Game.Syntax.Parser where

import Prelude

import Control.Alt ((<|>))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Game.Syntax.Spec (Expression(..), PlayerAction(..))
import Lib.Parser (Parser, anySpace, finiteString, runParser, someSpace, string)

actionParser :: Parser PlayerAction 
actionParser = move <|> take

move:: Parser PlayerAction
move = do
  _ <- anySpace
  _ <- string "move"
  _ <- (string " to ") <|> someSpace
  whereTo <- finiteString
  pure $ Move whereTo

take :: Parser PlayerAction
take = takeItemFrom <|> takeItem

takeItem :: Parser PlayerAction
takeItem = do
  _ <- anySpace
  _ <- string "take"
  _ <- someSpace
  itemName <- finiteString
  pure $ Take itemName

takeItemFrom:: Parser PlayerAction
takeItemFrom = do
  _ <- anySpace
  _ <- string "take"
  _ <- someSpace
  itemName <- finiteString
  _ <- someSpace
  _ <- string "from"
  _ <- someSpace
  fromWhere <- finiteString
  pure $ TakeFrom itemName fromWhere

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
  logShow $ runParser (expressionParser) "load game"