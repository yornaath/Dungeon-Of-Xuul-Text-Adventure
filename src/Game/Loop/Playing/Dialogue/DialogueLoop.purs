module Game.Loop.Playing.Dialogue.DialogueLoop where

import Prelude

import Data.Array (foldl, mapWithIndex)
import Data.Array as A
import Game.Data.Character (CharacterSheet(..))
import Game.Data.Dialogue (Dialogue, Reply, ChoicePoint)
import Data.Int (fromString)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Game.Engine (Engine, log, prompt)
import Game.GameState (GameState(..))
import Game.Loop.Playing.PlayingState (PlayingState)

dialogue :: PlayingState -> Dialogue -> Int -> Engine GameState
dialogue state dialogue' index = do
  let (CharacterSheet {name}) = state.character
  case M.lookup index dialogue' of 
    (Just choicePoint) -> do
      log $ renderChoicePoint choicePoint
      reply <- askForReply choicePoint
      log $ name <> ": “" <> reply.text <> "”\n"
      case reply.next of 
        (Just nextIndex) -> do 
          dialogue (state { turn = state.turn + 1}) dialogue' nextIndex
        (Nothing) -> do
          log $ "[dialogue ended]\n"
          pure (Playing state)
    _ -> do
      pure (Playing state)

askForReply :: ChoicePoint -> Engine Reply 
askForReply (Tuple text replies) = do
  input <- prompt
  case fromString input of 
    (Just index) -> do
      let reply = A.index replies (index - 1)
      case reply of 
        (Just reply') -> do 
          pure reply'
        (Nothing) -> do
          log "Not a valid choice. \n"
          askForReply (Tuple text replies)
    _ -> do
      askForReply (Tuple text replies)

renderChoicePoint :: ChoicePoint -> String
renderChoicePoint (Tuple text replies) =
  "\n“" <> text <> "”\n\n" <> foldl (\acc reply -> acc <> renderReply reply) "" (mapWithIndex (\index reply -> (Tuple index reply)) replies) <> "\n"

renderReply :: Tuple Int Reply -> String
renderReply (Tuple index reply) =
  (show (index + 1)) <> ". " <> reply.text <> "\n"

