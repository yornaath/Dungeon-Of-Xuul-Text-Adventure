module Game.Loop.Playing where
  
import Prelude

import Game.Game (Game)
import Game.GameState (GameState(..), PlayingState)

playing :: PlayingState -> Array String -> Game GameState
playing state input = do
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