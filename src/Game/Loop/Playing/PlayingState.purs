module Game.Loop.Playing.PlayingState where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Game.Data.Character (CharacterSheet)
import Game.Data.Location (Location)

type GlobalGameState a = {
  turn :: Int,
  character :: CharacterSheet,
  location :: Location
  | a
}

type ExplorationState = GlobalGameState ()
type CombatState = GlobalGameState ()
type DialogueState = GlobalGameState ()

data PlayingState 
  = Exploration ExplorationState
  | CombatMode CombatState
  | DialogueMode DialogueState

derive instance genericPlayingState:: Generic PlayingState _

instance showPlayingState :: Show PlayingState where
  show = genericShow

instance eqPlayingState :: Eq PlayingState where
  eq = genericEq

instance encodePlayingState :: EncodeJson PlayingState where
  encodeJson a = genericEncodeJson a

instance decodePlayingState :: DecodeJson PlayingState where
  decodeJson a = genericDecodeJson a

getCharacter :: PlayingState -> CharacterSheet
getCharacter playingState = 
  case playingState of 
    Exploration s' -> s'.character
    CombatMode s' -> s'.character
    DialogueMode s' -> s'.character

startGame :: CharacterSheet -> Location -> PlayingState
startGame character location = Exploration {
  turn: 0,
  character,
  location
}