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
import Data.Newtype (unwrap)
import Data.Role (Role)
import Data.Stats (Endurance(..), Stats(..))

data CharacterSheet = CharacterSheet 
  { name :: String,
    stats :: Stats,
    role :: Role,
    xp :: Experience
  }

derive instance genericCharacterSheet:: Generic CharacterSheet _

instance showCharacterSheet :: Show CharacterSheet where
  show (CharacterSheet c) =
    let (Stats stats) = c.stats in
      c.name <> " \n" <>
      "Class:         " <> show c.role <> "\n" <>
      "Experience:    " <> show c.xp <> "\n" <>
      "Agility:       " <> show stats.agi <> "\n" <>
      "Strength:      " <> show stats.str <> "\n" <>
      "Endurance:     " <> show stats.end <> "\n" <>
      "Wisdom:        " <> show stats.wis <> "\n" <>
      "Intelligence:  " <> show stats.int <> "\n"

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
    (Stats stats) = cs.stats
    (Level l) = levelof cs.xp
    (Endurance end) = stats.end
    hp' = (10 * l) * floor ((toNumber end) * 1.5)
  in
    hp'