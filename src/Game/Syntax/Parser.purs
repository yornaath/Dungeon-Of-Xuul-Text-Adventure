module Game.Syntax.Parser where

import Prelude

import Control.Alt ((<|>))
import Data.Traversable (sequence)
import Game.Syntax.Spec (CombatTurn(..), Expression(..), PlayerAction(..))
import Lib.Parser (Parser, anySpace, finiteString, literal, someSpace)

expressionParser :: Parser Expression
expressionParser = load <|> save <|> (Action <$> actionParser)

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


actionParser :: Parser PlayerAction 
actionParser = move <|> consume <|> take <|> (Combat <$> combatParser)

move :: Parser PlayerAction
move = do
  _ <- sequence [anySpace, literal "move", (literal " to ") <|> someSpace]
  whereTo <- finiteString
  pure $ Move whereTo

take :: Parser PlayerAction
take = takeItemFrom <|> takeItem

consume :: Parser PlayerAction
consume = do
  _ <- anySpace
  _ <- literal "consume"
  _ <- someSpace
  consumable <- finiteString
  pure $ Consume consumable

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


combatParser :: Parser CombatTurn
combatParser = attack <|> cast

attack :: Parser CombatTurn
attack = do
  _ <- anySpace
  _ <- (literal "attack") <|> (literal "hit")
  _ <- someSpace
  enemy <- finiteString
  pure $ Attack enemy

cast :: Parser CombatTurn
cast = do
  _ <- anySpace
  _ <- (literal "cast")
  _ <- someSpace
  spellName <- finiteString
  pure $ Cast spellName