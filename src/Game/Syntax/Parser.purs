module Game.Syntax.Parser where

import Prelude

import Control.Alt ((<|>))
import Data.Traversable (sequence)
import Game.Syntax.Spec (Expression(..), PlayerAction(..))
import Lib.Parser (Parser, anySpace, finiteString, literal, someSpace)

expressionParser :: Parser Expression
expressionParser = load <|> save <|> turn

load :: Parser Expression
load = do
  _ <- anySpace
  _ <- literal "load"
  _ <- someSpace
  saveGame <- finiteString
  pure (Load saveGame)

save :: Parser Expression
save = do
  _ <- anySpace
  _ <- literal "save"
  _ <- someSpace
  saveGame <- finiteString
  pure (Load saveGame)

turn :: Parser Expression
turn = do 
  action <- actionParser
  pure $ Turn action

actionParser :: Parser PlayerAction 
actionParser = move <|> take

move:: Parser PlayerAction
move = do
  _ <- sequence [anySpace, literal "move", (literal " to ") <|> someSpace]
  whereTo <- finiteString
  pure $ Move whereTo

take :: Parser PlayerAction
take = takeItemFrom <|> takeItem

takeItem :: Parser PlayerAction
takeItem = do
  _ <- sequence [anySpace, literal "take", someSpace]
  itemName <- finiteString
  pure $ Take itemName

takeItemFrom:: Parser PlayerAction
takeItemFrom = do
  _ <- sequence [anySpace, literal "take", someSpace]
  item <- finiteString
  _ <- sequence [anySpace, literal "from", someSpace]
  from <- finiteString
  pure $ TakeItemFrom item from