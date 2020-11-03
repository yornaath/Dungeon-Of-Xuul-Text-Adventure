module Game.Loop.Playing.Dialogue.DialogueLoop where

import Prelude

import Data.Array (foldl, mapWithIndex)
import Data.Array as A
import Data.Either (Either(..))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Game.Data.Dialogue (Dialogue, Reply, ChoicePoint)
import Game.Engine (Engine, log, prompt)
import Game.Loop.Playing.PlayingState (DialogueState)
import Game.Syntax.Parser (dialogueParser)
import Game.Syntax.Spec (DialogueTurn(..))
import Lib.Parser (runParser)

dialogueLoop :: DialogueState -> Dialogue -> Int -> Engine DialogueState
dialogueLoop state dialogue' index = do
  let character = state.character
  case M.lookup index dialogue' of 
    (Just choicePoint) -> do
      log $ renderChoicePoint choicePoint
      reply <- askForReply choicePoint
      log $ character.name <> ": “" <> reply.text <> "”\n"
      case reply.next of 
        (Just nextIndex) -> do 
          dialogueLoop (state { turn = state.turn + 1}) dialogue' nextIndex
        (Nothing) -> do
          log $ "[dialogue ended]\n"
          pure state
    _ -> do
      pure state

askForReply :: ChoicePoint -> Engine Reply 
askForReply (Tuple text replies) = do
  input <- prompt
  case runParser dialogueParser input of 
    Right (Tuple (Answer index) _)  -> do
      let reply = A.index replies (index - 1)
      case reply of 
        (Just reply') -> do 
          pure reply'
        (Nothing) -> do
          log "Not a valid choice. \n"
          askForReply (Tuple text replies)
    Left error -> do
      askForReply (Tuple text replies)

renderChoicePoint :: ChoicePoint -> String
renderChoicePoint (Tuple text replies) =
  "\n“" <> text <> "”\n\n" <> foldl (\acc reply -> acc <> renderReply reply) "" (mapWithIndex (\index reply -> (Tuple index reply)) replies) <> "\n"

renderReply :: Tuple Int Reply -> String
renderReply (Tuple index reply) =
  (show (index + 1)) <> ". " <> reply.text <> "\n"

