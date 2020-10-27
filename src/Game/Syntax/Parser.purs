module Game.Syntax.Parser where

import Prelude

import Control.Alt ((<|>))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Game.Syntax.Spec (Expression(..), PlayerAction(..))
import Lib.Parser (Parser, anySpace, finiteString, runParser, someSpace, string)

actionParser :: Parser PlayerAction 
actionParser = moveAction

moveAction:: Parser PlayerAction
moveAction = do
  _ <- anySpace
  _ <- string "move"
  _ <- (string " to ") <|> someSpace
  whereTo <- finiteString
  pure $ Move whereTo

expressionParser :: Parser Expression
expressionParser = loadExpression <|> saveExpression <|> (Turn <$> actionParser)

loadExpression :: Parser Expression
loadExpression = do
  _ <- anySpace
  _ <- string "load"
  _ <- someSpace
  saveGame <- finiteString
  pure (Load saveGame)

saveExpression :: Parser Expression
saveExpression = do
  _ <- anySpace
  _ <- string "save"
  _ <- someSpace
  saveGame <- finiteString
  pure (Load saveGame)

turnExpression :: Parser Expression
turnExpression = do 
  action <- actionParser
  pure $ Turn action

letStatement :: Parser (Array String)
letStatement = do
  declare <- string "let"
  _ <- someSpace
  name <- finiteString
  _ <- someSpace
  _ <- string "="
  _ <- someSpace
  value <- finiteString
  pure [name, value]

main :: Effect Unit
main = do
  logShow $ runParser (expressionParser) "move north"