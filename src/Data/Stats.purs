module Data.Stats where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.Newtype (class Newtype)

newtype Agility = Agility Int

derive instance newtypeAgility :: Newtype Agility _
derive newtype instance semiringAgility :: Semiring Agility
derive newtype instance encodeJsonAgility :: EncodeJson Agility
derive newtype instance decodeJsonAgility :: DecodeJson Agility
derive newtype instance showAgility :: Show Agility

newtype Strength = Strength Int

derive instance newtypeStrength :: Newtype Strength _
derive newtype instance semiringStrength :: Semiring Strength
derive newtype instance encodeJsonStrength :: EncodeJson Strength
derive newtype instance decodeJsonStrength :: DecodeJson Strength
derive newtype instance showStrength :: Show Strength

newtype Endurance = Endurance Int

derive instance newtypeEndurance :: Newtype Endurance _
derive newtype instance semiringEndurance :: Semiring Endurance
derive newtype instance encodeJsonEndurance :: EncodeJson Endurance
derive newtype instance decodeJsonEndurance :: DecodeJson Endurance
derive newtype instance showEndurance :: Show Endurance

newtype Wisdom = Wisdom Int

derive instance newtypeWisdom :: Newtype Wisdom _
derive newtype instance semiringWisdom :: Semiring Wisdom
derive newtype instance encodeJsonWisdom :: EncodeJson Wisdom
derive newtype instance decodeJsonWisdom :: DecodeJson Wisdom
derive newtype instance showWisdom :: Show Wisdom

newtype Intelligence = Intelligence Int

derive instance newtypeIntelligence :: Newtype Intelligence _
derive newtype instance semiringIntelligence :: Semiring Intelligence
derive newtype instance encodeJsonIntelligence :: EncodeJson Intelligence
derive newtype instance decodeJsonIntelligence :: DecodeJson Intelligence
derive newtype instance showIntelligence :: Show Intelligence

data Stats = Stats 
  { agi :: Agility,
    str :: Strength,
    end :: Endurance,
    wis :: Wisdom,
    int :: Intelligence
  }

derive instance genericStats :: Generic Stats _

instance showStats :: Show Stats where
  show = genericShow

instance semigroupStats :: Semigroup Stats where
  append (Stats a) (Stats b) = Stats { agi, str, end, wis, int } where
    agi = a.agi + b.agi
    str = a.str + b.str 
    end = a.end + b.end
    wis = a.wis + b.wis
    int = a.int + b.int

instance monoidStats :: Monoid Stats where
  mempty = emptyStats

instance encodeJsonStats :: EncodeJson Stats where
  encodeJson a = genericEncodeJson a

instance decodeJsonStats :: DecodeJson Stats where
  decodeJson a = genericDecodeJson a

mkStats :: Int -> Int -> Int -> Int -> Int -> Stats
mkStats agi str end wis int = Stats { 
  agi: Agility agi,
  str: Strength str,
  end: Endurance end,
  wis: Wisdom wis,
  int: Intelligence int
}

emptyStats :: Stats
emptyStats = Stats { 
  agi: Agility 0,
  str: Strength 0,
  end: Endurance 0,
  wis: Wisdom 0,
  int: Intelligence 0
}

total :: List Stats -> Stats
total statsList = foldl (<>) emptyStats statsList