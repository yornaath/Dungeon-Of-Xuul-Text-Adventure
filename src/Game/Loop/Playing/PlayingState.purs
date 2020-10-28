module Game.Loop.Playing.PlayingState where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Game.Data.Character (CharacterSheet)
import Game.Data.Location (Location)

type GlobalGameState l = {
  turn :: Int,
  character :: CharacterSheet,
  location :: Location
  | l
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