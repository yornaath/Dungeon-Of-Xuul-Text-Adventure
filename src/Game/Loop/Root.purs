module Game.Loop.Root where
  
import Prelude

import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Game.Game (Game, liftGame)
import Game.GameState (GameState(..))
import Game.Loop.CharacterCreation (characterCreation)
import Game.Loop.Playing (playing)
import Game.Loop.MainMenu (mainMenu)
import Game.Saving (loadGame, saveGame)

game :: GameState -> Array String -> Game GameState
game state input = do
  case state of
    (MainMenu) -> do
      newState <- liftGame $ mainMenu state input
      pure newState
    (CreatingCharacter state') -> do 
      newState <- liftGame $ characterCreation state' input
      pure newState
    (Playing state') -> do
      newState <- liftGame $ playing state' input
      pure newState
  
      