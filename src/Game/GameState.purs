module Game.GameState where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Character (CharacterSheet)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Location (Location)
import Data.Maybe (Maybe, maybe)
import Data.Role (Role)

type PlayingState = {
  character :: CharacterSheet,
  location :: Location
}

type CreatingCharacterState = {
  name :: Maybe String,
  role :: Maybe Role
}

showCharacterCreationCompletionState :: CreatingCharacterState -> String
showCharacterCreationCompletionState { name, role } =
  "Name: " <> (maybe " required" (\n -> n <> " ✓") name) <> "\n" <>
  "Role: " <> (maybe " required" (\r -> show r <> " ✓") role)

data GameState = 
    MainMenu
  | Playing PlayingState
  | CreatingCharacter CreatingCharacterState

derive instance genericGameState :: Generic GameState _

instance encodeJsonGameState :: EncodeJson GameState where
  encodeJson a = genericEncodeJson a

instance decodeJsonGameState :: DecodeJson GameState where
  decodeJson a = genericDecodeJson a


gameStateToJson :: GameState -> Json
gameStateToJson = encodeJson

gameStateFromJson :: Json -> Either JsonDecodeError GameState
gameStateFromJson = decodeJson