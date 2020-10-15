module Game.Loop.MainMenu where 

import Prelude

import Control.Monad.Reader (ask)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Game.Game (Game)
import Game.GameState (GameState)
import Game.Saving (loadGame)
import Lib.AffReadline (question)

mainMenu :: GameState -> Array String -> Game GameState
mainMenu state input = do
  case input of
    ["load", save] -> do 
      loadedSaveState <- liftAff $ loadGame save
      log $ "loaded game: " <> save
      pure loadedSaveState
    _ -> do 
      log "I dont understand."
      pure state

