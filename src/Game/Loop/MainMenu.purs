module Game.Loop.MainMenu where 

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String (split)
import Effect.Aff.Class (liftAff)
import Engine.SaveGames (loadGame)
import Game.Engine (Engine, prompt, log)
import Game.GameState (GameState(..))
import Static.Text as StaticText

mainMenu :: GameState -> Array String -> Engine GameState
mainMenu state input = do
  log StaticText.banner
  mainMenuChoices state input

mainMenuChoices :: GameState -> Array String -> Engine GameState
mainMenuChoices state input = do
  case input of
    ["help"] -> do
      log ":load savename -- Load a savegame by its name"
      log ":start -- Start a new game"
      mainMenuChoices state []
    [":load", save] -> do 
      loadedSaveState <- liftAff $ loadGame save
      log $ "loaded game: " <> save
      pure loadedSaveState
    [":start"] -> do
      pure (CreatingCharacter { name: Nothing, role: Nothing })
    [] -> do
      input' <- prompt
      let command = (split (wrap " ")) input'
      mainMenuChoices state command
    _ -> do 
      log "I dont understand."
      mainMenuChoices state []

