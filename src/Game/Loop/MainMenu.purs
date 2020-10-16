module Game.Loop.MainMenu where 

import Prelude

import Control.Monad.Reader (ask)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Game.Game (Game, liftGame)
import Game.GameState (GameState(..))
import Game.Saving (loadGame)
import Lib.AffReadline (command, question)

mainMenu :: GameState -> Array String -> Game GameState
mainMenu state input = do
  case input of
    ["help"] -> do
      log ":load savename -- Load a savegame by its name"
      log ":start -- Start a new game"
      pure state
    [":load", save] -> do 
      loadedSaveState <- liftAff $ loadGame save
      log $ "loaded game: " <> save
      pure loadedSaveState
    [":start"] -> do
      pure (CreatingCharacter { name: Nothing, role: Nothing })
    [] -> do
      { interface } <- ask
      input' <- liftAff $ command interface "> "
      mainMenu state input'
    _ -> do 
      log "I dont understand."
      pure state

