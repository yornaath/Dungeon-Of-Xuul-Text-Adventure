module Game.Loop.Playing.Dialogue.DialogueLoop where

import Prelude

import Control.Monad.Reader (ask)
import Data.Array as A
import Data.Dialogue (Dialogue, Reply, ChoicePoint, renderChoicePoint)
import Data.Int (fromString)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Game.Engine (Engine)
import Game.GameState (GameState(..))
import Game.Loop.Playing.PlayingState (PlayingState)
import Lib.AffReadline (command, question)

dialogue :: PlayingState -> Dialogue -> Int -> Engine GameState
dialogue state dialogue' index = do
  let start = M.lookup index dialogue'
  case start of 
    (Just choicePoint) -> do
      log $ renderChoicePoint choicePoint
      reply <- askForReply choicePoint
      log $ "You: “" <> reply.text <> "”\n"
      case reply.next of 
        (Just nextIndex) -> do 
          dialogue state dialogue' nextIndex
        (Nothing) -> do
          log "dialogue ended\n"
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

