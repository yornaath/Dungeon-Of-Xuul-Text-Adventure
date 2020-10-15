module Game.Loop.Root where
  
import Prelude

import Game.Game (Game, liftGame)
import Game.GameState (GameState(..))
import Game.Loop.CharacterCreation (characterCreation)
import Game.Loop.Playing (playing)

game :: GameState -> Array String -> Game GameState
game state input = do
  case state of
    (CreatingCharacter state') -> do 
      newState <- liftGame $ characterCreation state' input
      pure newState
    (Playing state') -> do
      newState <- liftGame $ playing state' input
      pure newState