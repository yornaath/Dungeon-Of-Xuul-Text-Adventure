module Game.Loop.Root where
  
import Prelude

import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Game.Game (Game, liftGame)
import Game.GameState (GameState(..))
import Game.Loop.CharacterCreation (characterCreation)
import Game.Loop.MainMenu (mainMenu)
import Game.Loop.Playing (playing)
import Game.Saving (loadGame, saveGame)
import Lib.AffReadline (command)

game :: GameState -> Game GameState
game state = do
  case state of
    (MainMenu) -> do
      newState <- liftGame $ mainMenu state []
      pure newState
    (CreatingCharacter state') -> do 
      newState <- liftGame $ characterCreation state' []
      pure newState
    (Playing state') -> do
      newState <- liftGame $ playing state' []
      pure newState
  
      