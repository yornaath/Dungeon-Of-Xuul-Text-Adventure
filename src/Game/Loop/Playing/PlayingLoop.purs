module Game.Loop.Playing.PlayingLoop where

import Prelude

import Control.Monad.Reader (ask)
import Data.Character (CharacterSheet)
import Data.Location (Location)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log, logShow)
import Game.Engine (Engine)
import Game.GameState (GameState(..))
import Game.Saving (saveGame)
import Lib.AffReadline (command)
import Game.Loop.Playing.PlayingState (PlayingState)

startGame :: CharacterSheet -> Location -> PlayingState
startGame character location = {
  turn: 0,
  character,
  location
}

playing :: PlayingState -> Array String -> Engine GameState
playing state input = do
  case input of
    ["help"] -> do
      log ":save savename -- Save a game by its name"
      log ":exit -- Exit to main menu"
      log ":c -- Inspect character"
      pure (Playing state)
    [":exit"] -> do
      pure (MainMenu)
    [":save", save] -> do
      saved <- liftAff $ saveGame save (Playing state)
      pure (Playing state)
    [":c"] -> do 
      logShow state.character
      pure (Playing state)
    [] -> do
      { interface } <- ask
      input' <- liftAff $ command interface "> "
      playing state input'
    _ -> do
      pure (Playing state)