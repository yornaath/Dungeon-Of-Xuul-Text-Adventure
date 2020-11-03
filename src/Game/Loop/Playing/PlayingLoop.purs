module Game.Loop.Playing.PlayingLoop where

import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Game.Data.Dialogue (Dialogue)
import Game.Engine (Engine, log)
import Game.GameState (GameState)
import Game.Loop.Playing.Exploration (explorationLoop)
import Game.Loop.Playing.PlayingState (PlayingState(..))
import Game.Syntax.Spec (PlayerAction(..))
import Prelude (discard)

playing :: PlayingState -> PlayerAction -> Engine GameState
playing state action = do
  case state of 
    Exploration exploringState -> do 
      explorationLoop exploringState (Look)
    CombatMode _ -> do
      log "[Combat not yet supported]"
      playing state Idle
    DialogueMode _ -> do
      log "[Dialogue not yet supported]"
      playing state Idle