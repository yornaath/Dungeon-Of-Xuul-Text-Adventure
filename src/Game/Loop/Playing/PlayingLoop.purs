module Game.Loop.Playing.PlayingLoop where

import Prelude

import Data.Dialogue (Dialogue)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String (split)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Engine.SaveGames (saveGame)
import Game.Engine (Engine, prompt, log)
import Game.GameState (GameState(..))
import Game.Loop.Playing.Dialogue.DialogueLoop (dialogue)
import Game.Loop.Playing.PlayingState (PlayingState)

playing :: PlayingState -> Array String -> Engine GameState
playing state input = do
  case input of
    ["help"] -> do
      log ":save savename -- Save a game by its name"
      log ":exit -- Exit to main menu"
      log ":c -- Inspect character"
      pure (Playing state)
    [":exit"] -> do
      pure (MainMenu)
    [":save", save] -> do
      saved <- liftAff $ saveGame save (Playing state)
      pure (Playing state)
    [":c"] -> do 
      log $ show state.character
      pure (Playing state)
    ["talk"] -> do
      dialogue state testDialogue 1
    [] -> do
      input' <- prompt
      let command = (split (wrap " ")) input'
      playing state command
    _ -> do
      pure (Playing state)


testDialogue :: Dialogue
testDialogue = M.fromFoldable [
  (Tuple 1 
    (Tuple "Howdy there partner, how are you?" [
      { text: "I am fine... How did you get here?", next: Just 2 },
      { text: "Kind of tired to be honest... But have to bear on right?", next: Just 3 }
    ])
  ),
  (Tuple 2
    (Tuple "I have been here for a million years son." [
      { text: "Well ok then....", next: Nothing }
    ])
  ),
  (Tuple 3
    (Tuple "Right you are. Carry on." [
      { text: "«carry on»", next: Nothing }
    ])
  )
]