module Game.Loop.Root where
  
import Prelude

import Control.Apply (lift2)
import Control.Monad.RWS (ask, get, lift)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff)
import Effect.Aff.AVar (put, read, take)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
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