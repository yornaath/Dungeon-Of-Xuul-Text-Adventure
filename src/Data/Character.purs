module Data.Character where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Experience (Experience, Level(..), levelof)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (floor, toNumber)
import Data.Role (Role)
import Data.Stats (Endurance(..), Stats)

data CharacterSheet = CharacterSheet 
  { name :: String,
    stats :: Stats,
    role :: Role,
    xp :: Experience
  }

derive instance genericCharacterSheet:: Generic CharacterSheet _

instance showCharacterSheet :: Show CharacterSheet where
  show = genericShow

instance encodeJsonCharacterSheet :: EncodeJson CharacterSheet where
  encodeJson a = genericEncodeJson a

instance decodeJsonCharacterSheet :: DecodeJson CharacterSheet where
  decodeJson a = genericDecodeJson a

mkCharacterSheet :: String -> Role -> Stats -> Experience -> CharacterSheet
mkCharacterSheet name role stats xp = CharacterSheet {
  name,
  role,
  stats,
  xp
}

maxhp :: CharacterSheet -> Int
maxhp (CharacterSheet cs) = 
  let 
    (Level l) = levelof cs.xp
    (Endurance end) = cs.stats.end
    hp' = (10 * l) * floor ((toNumber end) * 1.5)
  in
    hp'