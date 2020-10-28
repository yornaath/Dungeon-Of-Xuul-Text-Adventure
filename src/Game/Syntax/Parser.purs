module Game.Syntax.Parser where

import Prelude

import Control.Alt ((<|>))
import Data.Traversable (sequence)
import Game.Syntax.Spec (CombatTurn(..), DialogueTurn(..), Expression(..), PlayerAction(..))
import Lib.Parser (Parser, anySpace, finiteString, literal, someSpace)

--
-- Root level Expression parser
--

expressionParser :: Parser Expression
expressionParser 
  =   load 
  <|> save
  <|> exit
  <|> (Action <$> actionParser)

load :: Parser Expression
load = do
  _ <- anySpace
  _ <- literal ":load"
  _ <- someSpace
  saveGame <- finiteString
  pure (Load saveGame)

save :: Parser Expression
save = do
  _ <- anySpace
  _ <- literal ":save"
  _ <- someSpace
  saveGame <- finiteString
  pure (Save saveGame)

exit :: Parser Expression
exit = do
  _ <- anySpace
  _ <- literal ":exit"
  _ <- someSpace
  saveGame <- finiteString
  pure Exit



--
-- Player Actions parser
--

actionParser :: Parser PlayerAction 
actionParser 
  =   move
  <|> look
  <|> consume 
  <|> take 
  <|> openCharacterSheet
  <|> (Combat <$> combatParser) 
  <|> (Dialogue <$> dialogueParser)

move :: Parser PlayerAction
move = do
  _ <- sequence [anySpace, literal "move", (literal " to ") <|> someSpace]
  whereTo <- finiteString
  pure $ Move whereTo

look :: Parser PlayerAction
look = do
  _ <- anySpace
  _ <- literal "look"
  _ <- anySpace
  pure $ Look

take :: Parser PlayerAction
take = takeItemFrom <|> takeItem

openCharacterSheet :: Parser PlayerAction
openCharacterSheet = do
  _ <- anySpace
  _ <- literal ":c" <|> literal "char" <|> literal "character"
  _ <- anySpace
  pure $ OpenCharacterSheet

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



--
-- Combat Actions parser
--

combatParser :: Parser CombatTurn
combatParser 
  =   attack 
  <|> cast 
  <|> defend

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

defend :: Parser CombatTurn
defend = do
  _ <- anySpace
  _ <- (literal "defend")
  _ <- anySpace
  pure $ Defend


--
-- Dialogue Actions parser
--

dialogueParser :: Parser DialogueTurn
dialogueParser = answer

answer :: Parser DialogueTurn
answer = do
  _ <- anySpace
  _ <- literal "answer:"
  _ <- someSpace
  answer' <- finiteString
  pure $ Answer answer'
