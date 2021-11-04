module Game.GameState where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Generic.Rep (class Generic)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Data.Either (Either)
import Game.Loop.CharacterCreation.CreatingCharacterState (CreatingCharacterState)
import Game.Loop.Playing.PlayingState (PlayingState)

data GameState = 
    MainMenu
  | Playing PlayingState
  | CreatingCharacter CreatingCharacterState

derive instance genericGameState :: Generic GameState _

instance showGameState :: Show GameState where
  show state = genericShow state

instance encodeJsonGameState :: EncodeJson GameState where
  encodeJson a = genericEncodeJson a

instance decodeJsonGameState :: DecodeJson GameState where
  decodeJson a = genericDecodeJson a

gameStateToJson :: GameState -> Json
gameStateToJson = encodeJson

gameStateFromJson :: Json -> Either JsonDecodeError GameState
gameStateFromJson = decodeJson