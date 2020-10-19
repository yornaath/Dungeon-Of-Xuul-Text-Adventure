module Game.Loop.Playing.Dialogue.DialogueLoop where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (foreground, italic, withGraphics)
import Control.Monad.Reader (ask)
import Data.Array (foldl, mapWithIndex)
import Data.Array as A
import Data.Character (CharacterSheet(..))
import Data.Dialogue (Dialogue, Reply, ChoicePoint)
import Data.Int (fromString)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Game.Engine (Engine)
import Game.GameState (GameState(..))
import Game.Loop.Playing.PlayingState (PlayingState)
import Lib.AffReadline (question)

dialogue :: PlayingState -> Dialogue -> Int -> Engine GameState
dialogue state dialogue' index = do
  let (CharacterSheet {name}) = state.character
  case M.lookup index dialogue' of 
    (Just choicePoint) -> do
      log $ renderChoicePoint choicePoint
      reply <- askForReply choicePoint
      log $ withGraphics (foreground Blue) $ "\n" <> name <> " “" <> reply.text <> "”"
      case reply.next of 
        (Just nextIndex) -> do 
          dialogue (state { turn = state.turn + 1}) dialogue' nextIndex
        (Nothing) -> do
          log $ withGraphics (italic <> foreground White) $ "[dialogue ended]\n"
          pure (Playing state)
    _ -> do
      pure (Playing state)

askForReply :: ChoicePoint -> Engine Reply 
askForReply (Tuple text replies) = do
  { interface } <- ask
  input <- liftAff $ question interface "> "
  case fromString input of 
    (Just index) -> do
      let reply = A.index replies (index - 1)
      case reply of 
        (Just reply') -> do 
          pure reply'
        (Nothing) -> do
          log "Not a valid choice."
          askForReply (Tuple text replies)
    _ -> do
      askForReply (Tuple text replies)

renderChoicePoint :: ChoicePoint -> String
renderChoicePoint (Tuple text replies) =
  withGraphics (foreground Blue) $
  "\n“" <> text <> "”\n\n" <> foldl (\acc reply -> acc <> renderReply reply) "" (mapWithIndex (\index reply -> (Tuple index reply)) replies)

renderReply :: Tuple Int Reply -> String
renderReply (Tuple index reply) =
  withGraphics (foreground Yellow) $
  (show (index + 1)) <> ". " <> reply.text <> "\n"

