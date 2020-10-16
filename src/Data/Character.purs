module Data.Character where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Experience (Experience, Level(..), levelof)
import Data.Generic.Rep (class Generic)
import Data.Int (floor, toNumber)
import Data.Role (Role)
import Data.Stats (Endurance(..), Stats(..))

data CharacterSheet = CharacterSheet 
  { name :: String,
    stats :: Stats,
    role :: Role,
    xp :: Experience,
    hp :: Int
  }

derive instance genericCharacterSheet:: Generic CharacterSheet _

instance showCharacterSheet :: Show CharacterSheet where
  show (CharacterSheet c) =
    let 
      (Stats stats) = c.stats
      maxhp' = maxhp (CharacterSheet c)
      in
        c.name <> " \n" <>
        "Level:         " <> show (levelof c.xp) <> "\n" <>
        "Class:         " <> show c.role <> "\n" <>
        "Experience:    " <> show c.xp <> "\n" <>
        "Agility:       " <> show stats.agi <> "\n" <>
        "Strength:      " <> show stats.str <> "\n" <>
        "Endurance:     " <> show stats.end <> "\n" <>
        "Wisdom:        " <> show stats.wis <> "\n" <>
        "Intelligence:  " <> show stats.int <> "\n" <> 
        "HP:            " <> (show c.hp) <> "/" <> (show maxhp') <> "\n"

instance encodeJsonCharacterSheet :: EncodeJson CharacterSheet where
  encodeJson a = genericEncodeJson a

instance decodeJsonCharacterSheet :: DecodeJson CharacterSheet where
  decodeJson a = genericDecodeJson a

mkCharacterSheet :: String -> Role -> Stats -> Experience -> CharacterSheet
mkCharacterSheet name role stats xp =
  let 
    sheet = {
      name,
      role,
      stats,
      xp,
      hp: 0
    }
    hp = maxhp (CharacterSheet sheet)
  in
  CharacterSheet (sheet { hp = hp })

maxhp :: CharacterSheet -> Int
maxhp (CharacterSheet cs) = 
  let
    (Stats stats) = cs.stats
    (Level l) = levelof cs.xp
    (Endurance end) = stats.end
    hp' = (10 * l) * floor ((toNumber end) * 1.5)
  in
    hp'