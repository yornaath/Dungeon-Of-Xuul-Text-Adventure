module Game.Loop.Playing where
  
import Prelude

import Control.Monad.Reader (ask)
import Data.Character (CharacterSheet(..))
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log, logShow)
import Game.Game (Game)
import Game.GameState (GameState(..), PlayingState)
import Game.Saving (saveGame)
import Lib.AffReadline (command)
import Static.Text as Static.Text

playing :: PlayingState -> Array String -> Game GameState
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
    
-- game ["look"] = do
--   GameState state <- get
--   let (Location currentLocation) = state.location
--   let exits = (map (\(Exit e) -> ":exit " <> "(" <> e.name <> ") " <> e.description <> "\n") currentLocation.exits)
--   tell ((L.fromFoldable [
--     currentLocation.name,
--     "-------------------------------------------",
--     currentLocation.description <> "\n"
--   ]) <> exits)

-- game ["move", locationName] = do
--   GameState state <- get
--   let (Location currentLocation) = state.location
--   let maybeExit = L.find (\(Exit e) -> e.name == locationName) currentLocation.exits
--   case maybeExit of
--     Nothing -> tell (L.singleton ("No location by the name: " <> locationName))
--     Just (Exit exit) -> do
--       put $ GameState state { location = exit.location }

-- game [] = pure unit  

-- game _ = do