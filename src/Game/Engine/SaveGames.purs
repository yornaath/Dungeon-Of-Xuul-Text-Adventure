module Engine.SaveGames where

import Prelude

import Data.Argonaut.Core as AC
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, makeAff)
import Effect.Class.Console (log, logShow)
import Game.GameState (GameState, gameStateFromJson)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

loadGame :: String -> Aff GameState
loadGame save = makeAff \cb -> do
  -- readSavedStateString <- try $ FS.readTextFile UTF8 (Path.concat [__dirname, "../../.saves", save <> ".json"])
  w <- window
  storage <- localStorage w
  readSavedStateString <- getItem save storage
  case readSavedStateString of
    Nothing -> do 
      log $ ("Could not find savegame: " <> save)
    Just savedStateString -> do
      case jsonParser $ savedStateString of 
        Left error -> do
          log ("error :" <> error)
          pure unit
        Right json -> do
          case gameStateFromJson json of
            Left error -> do logShow error
            Right gameState -> do
              cb (Right gameState)
  mempty

saveGame :: String -> GameState -> Aff GameState
saveGame save currentState = makeAff \cb -> do
  let 
    gameStateJson = encodeJson currentState
    gameStateString = AC.stringifyWithIndent 2 gameStateJson
  w <- window
  storage <- localStorage w
  saved <- setItem save gameStateString storage
  --saved <- try $ FS.writeTextFile UTF8 (Path.concat [__dirname, "../../.saves", save <> ".json"]) gameStateString
  cb (Right currentState)
  mempty

-- module Engine.SaveGames where

-- import Prelude

-- import Data.Argonaut.Core as AC
-- import Data.Argonaut.Encode (encodeJson)
-- import Data.Argonaut.Parser (jsonParser)
-- import Data.Either (Either(..))
-- import Effect.Aff (Aff, makeAff)
-- import Effect.Class.Console (log, logShow)
-- import Effect.Exception (try)
-- import Game.GameState (GameState, gameStateFromJson)
-- import Node.Encoding (Encoding(..))
-- import Node.FS.Sync as FS
-- import Node.Globals (__dirname)
-- import Node.Path as Path

-- loadGame :: String -> Aff GameState
-- loadGame save = makeAff \cb -> do
--   readSavedStateString <- try $ FS.readTextFile UTF8 (Path.concat [__dirname, "../../.saves", save <> ".json"])
--   case readSavedStateString of
--     Left error -> do logShow error
--     Right savedStateString -> do
--       case jsonParser $ savedStateString of 
--         Left error -> do
--           log ("error :" <> error)
--           pure unit
--         Right json -> do
--           case gameStateFromJson json of
--             Left error -> do logShow error
--             Right gameState -> do
--               cb (Right gameState)
--   mempty

-- saveGame :: String -> GameState -> Aff GameState
-- saveGame save currentState = makeAff \cb -> do
--   let 
--     gameStateJson = encodeJson currentState
--     gameStateString = AC.stringifyWithIndent 2 gameStateJson
--   saved <- try $ FS.writeTextFile UTF8 (Path.concat [__dirname, "../../.saves", save <> ".json"]) gameStateString
--   cb (Right currentState)
--   mempty