module Data.Stats where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

newtype Agility = Agility Int

derive instance genericAgility:: Generic Agility _

instance encodeJsonAgility :: EncodeJson Agility where
  encodeJson a = genericEncodeJson a

instance decodeJsonAgility :: DecodeJson Agility where
  decodeJson a = genericDecodeJson a

instance showAgility :: Show Agility where
  show = genericShow

newtype Strength = Strength Int

derive instance genericStrength :: Generic Strength _

instance encodeJsonStrength :: EncodeJson Strength where
  encodeJson a = genericEncodeJson a

instance decodeJsonStrength :: DecodeJson Strength where
  decodeJson a = genericDecodeJson a

instance showStrength :: Show Strength where
  show = genericShow

newtype Endurance = Endurance Int

derive instance genericEndurance :: Generic Endurance _

instance encodeJsonEndurance :: EncodeJson Endurance where
  encodeJson a = genericEncodeJson a

instance decodeJsonEndurance :: DecodeJson Endurance where
  decodeJson a = genericDecodeJson a

instance showEndurance :: Show Endurance where
  show = genericShow

newtype Wisdom = Wisdom Int

derive instance genericWisdom :: Generic Wisdom _

instance encodeJsonWisdom :: EncodeJson Wisdom where
  encodeJson a = genericEncodeJson a

instance decodeJsonWisdom :: DecodeJson Wisdom where
  decodeJson a = genericDecodeJson a

instance showWisdom :: Show Wisdom where
  show = genericShow

newtype Intelligence = Intelligence Int

derive instance genericIntelligence :: Generic Intelligence _

instance encodeJsonIntelligence :: EncodeJson Intelligence where
  encodeJson a = genericEncodeJson a

instance decodeJsonIntelligence :: DecodeJson Intelligence where
  decodeJson a = genericDecodeJson a

instance showIntelligence :: Show Intelligence where
  show = genericShow

type Stats = 
  { agi :: Agility,
    str :: Strength,
    end :: Endurance,
    wis :: Wisdom,
    int :: Intelligence
  }

mkStats :: Int -> Int -> Int -> Int -> Int -> Stats
mkStats agi str end wis int =  { 
  agi: Agility agi,
  str: Strength str,
  end: Endurance end,
  wis: Wisdom wis,
  int: Intelligence int
}

emptyStats :: Stats
emptyStats = { 
  agi: Agility 0,
  str: Strength 0,
  end: Endurance 0,
  wis: Wisdom 0,
  int: Intelligence 0
}