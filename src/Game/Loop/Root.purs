module Game.Loop.Root where
  
import Prelude

import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Game.Game (Game, liftGame)
import Game.GameState (GameState(..))
import Game.Loop.CharacterCreation (characterCreation)
import Game.Loop.Playing (playing)
import Game.Saving (loadGame, saveGame)

game :: GameState -> Array String -> Game GameState
game state input = do
  case state of
    (CreatingCharacter state') -> do 
      newState <- liftGame $ characterCreation state' input
      pure newState
    (Playing state') -> do
      newState <- liftGame $ playing state' input
      pure newState
  -- case input of
  --   ["load", save] -> do 
  --     loadedSaveState <- liftAff $ loadGame save
  --     log $ "loaded game: " <> save
  --     pure loadedSaveState
  --   ["save", save] -> do
  --     saveState <- liftAff $ saveGame save state
  --     log $ "saved game: " <> save
  --     pure saveState
  --   _ -> do
      