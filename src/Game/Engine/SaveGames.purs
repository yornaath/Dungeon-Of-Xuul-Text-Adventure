module Engine.SaveGames where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as AC
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, decodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Milliseconds(..), makeAff)
import Effect.Class.Console (log, logShow)
import Effect.Now (now)
import Game.GameState (GameState)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

newtype SaveGame = SaveGame {
  gameState :: GameState,
  saved :: Number
}

derive instance genericSaveGame:: Generic SaveGame _

instance encodeJsonSaveGame :: EncodeJson SaveGame where
  encodeJson a = genericEncodeJson a

instance decodeJsonSaveGame :: DecodeJson SaveGame where
  decodeJson a = genericDecodeJson a

saveGameToJson :: SaveGame -> Json
saveGameToJson = encodeJson

saveGameFromJson :: Json -> Either JsonDecodeError SaveGame
saveGameFromJson = decodeJson

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
          case saveGameFromJson json of
            Left error -> do logShow error
            Right (SaveGame saveGame') -> do
              cb (Right saveGame'.gameState)
  mempty

saveGame :: String -> GameState -> Aff GameState
saveGame save currentState = makeAff \cb -> do
  time <- now
  let
    (Milliseconds ms) = unInstant $ time
    gameStateJson = saveGameToJson (SaveGame { gameState: currentState, saved: ms })
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