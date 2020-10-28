module Game.Loop.MainMenu where 

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String (split)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Engine.SaveGames (continue, loadGame)
import Game.Data.Character (CharacterSheet(..))
import Game.Engine (Engine, liftEngine, log, prompt)
import Game.GameState (GameState(..))
import Game.Loop.Playing.PlayingState (getCharacter)
import Static.Text as StaticText

mainMenu :: GameState -> Array String -> Engine GameState
mainMenu state input = do
  log StaticText.banner
  logHelp
  mainMenuChoices state input

mainMenuChoices :: GameState -> Array String -> Engine GameState
mainMenuChoices state input = do
  case input of
    ["help"] -> do
      logHelp
      mainMenuChoices state []
    [":load", save] -> do 
      loadedSaveState <- liftAff $ loadGame save
      log $ "loaded game: " <> save <> "\n"
      pure loadedSaveState
    [":load", save] -> do 
      loadedSaveState <- liftAff $ loadGame save
      log $ "loaded game: " <> save <> "\n"
      pure loadedSaveState
    [":start"] -> do
      pure (CreatingCharacter { name: Nothing, role: Nothing })
    [] -> do
      input' <- prompt
      let command = (split (wrap " ")) input'
      case command of 
        [""] -> do
          lastSave <- liftAff continue
          case lastSave of 
            Right saveState -> do 
              case saveState of 
                Playing playingState -> do
                  let (CharacterSheet character) = getCharacter playingState
                  log $ "Continue game character: " <> character.name <> " \n"
                  pure saveState
                _ -> do 
                  pure saveState
            Left _ -> do
              log "No save to continue.. \n"
              mainMenuChoices state []
        command' ->
          mainMenuChoices state command
    _ -> do 
      log "I dont understand."
      mainMenuChoices state []

logHelp :: Engine Unit
logHelp = do 
  log ":start           -- Start a new game"
  log ":load savename   -- Load a savegame by its name \n"
  log "Press enter [↵] to continue \n"