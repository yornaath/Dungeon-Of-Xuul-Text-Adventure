module Game.Syntax.Parser where

import Prelude

import Control.Alt ((<|>))
import Data.Array (concat, length, (..))
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Class.Console (logShow, time, timeEnd)
import Game.Syntax.Spec (Expression(..), PlayerAction(..))
import Lib.Parser (Parser, anySpace, finiteString, runParser, someSpace, literal)

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

main :: Effect Unit
main = do
  time "parsing"
  let expressions = concat $ ((\_ -> ["move to north", "take item from bag", "load game", "save game"]) <$> 1..9999)
  let parseResults = (runParser (expressionParser)) <$> expressions
  logShow $ length parseResults
  timeEnd "parsing"
    --logShow $ runParser (sequence [string "foo", string "bar"]) "foobar"