module Game.Loop.Root where
  
import Prelude

import Game.Engine (Engine)
import Game.GameState (GameState(..))
import Game.Loop.CharacterCreation (characterCreation)
import Game.Loop.MainMenu (mainMenu)
import Game.Loop.Playing.PlayingLoop (playing)

gameLoop :: GameState -> Engine GameState
gameLoop state = do
  case state of
    (MainMenu) -> do
      newState <- mainMenu state []
      pure newState
    (CreatingCharacter state') -> do 
      newState <- characterCreation state' []
      pure newState
    (Playing state') -> do
      newState <- playing state' []
      pure newState
  
      