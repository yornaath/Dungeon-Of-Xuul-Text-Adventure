module Data.Character where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Either (Either(..))
import Data.Experience (Experience, Level(..), levelof, unLevel)
import Data.Foldable (find, foldl)
import Data.Generic.Rep (class Generic)
import Data.Int (floor, toNumber)
import Data.Item (Item(..), unItem)
import Data.Maybe (Maybe(..))
import Data.Role (Role)
import Data.Stats (Endurance(..), Stats(..), emptyStats, mkStats)

data CharacterSheet = CharacterSheet 
  { name        :: String,
    stats       :: Stats,
    role        :: Role,
    xp          :: Experience,
    hp          :: Int,
    inventory   :: Array Item,
    equipped    :: Equipment
  }

type Equipment = {
  helmet  :: Maybe Item,
  chest   :: Maybe Item,
  hands   :: Maybe Item,
  leggs   :: Maybe Item,
  feet    :: Maybe Item
}

derive instance genericCharacterSheet:: Generic CharacterSheet _

instance showCharacterSheet :: Show CharacterSheet where
  show (CharacterSheet c) =
    let 
      (Stats stats) = totalStats $ CharacterSheet c
      maxhp' = maxhp (CharacterSheet c)
      in
        "Name:          " <> c.name                               <> "\n" <>
        "Level:         " <> show (levelof c.xp)                  <> "\n" <>
        "Class:         " <> show c.role                          <> "\n" <>
        "Experience:    " <> show c.xp                            <> "\n" <>
        "Agility:       " <> show stats.agi                       <> "\n" <>
        "Strength:      " <> show stats.str                       <> "\n" <>
        "Endurance:     " <> show stats.end                       <> "\n" <>
        "Wisdom:        " <> show stats.wis                       <> "\n" <>
        "Intelligence:  " <> show stats.int                       <> "\n" <> 
        "HP:            " <> (show c.hp) <> "/" <> (show maxhp')  <> "\n"

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
      hp: 0,
      inventory: [],
      equipped: {
        helmet: Just $ Helmet { name: "Howler", description: "Nasty looking thing", stats: mkStats 2 4 0 0 0, levelRequirement: 0},
        chest: Nothing,
        hands: Nothing,
        leggs: Nothing,
        feet: Nothing
      }
    }
    hp = maxhp (CharacterSheet sheet)
  in
    CharacterSheet (sheet { hp = hp })


maxhp :: CharacterSheet -> Int
maxhp characterSheet = 
  let
    (CharacterSheet sheet') = characterSheet
    (Stats stats) = sheet'.stats
    (Level l) = levelof sheet'.xp
    (Stats totalStats) = totalStats characterSheet
    (Endurance end) = totalStats.end
    hp' = (10 * l) * floor ((toNumber end) * 1.5)
  in
    hp'


equippedStats :: Equipment -> Array Stats
equippedStats equipment = itemStatsOrEmpty <$> equipedItems where
  itemStatsOrEmpty item = case item of  
    Just item' -> (unItem item').stats 
    Nothing -> emptyStats
  equipedItems = [
    equipment.helmet,
    equipment.chest,
    equipment.hands,
    equipment.leggs,
    equipment.feet
  ]


totalStats :: CharacterSheet -> Stats
totalStats (CharacterSheet sheet) = foldl (<>) emptyStats allStats
  where 
    allStats = [sheet.stats] <> (equippedStats sheet.equipped)


hasItem :: Item -> CharacterSheet -> Boolean
hasItem item (CharacterSheet sheet) = 
  case find (\item' -> item' == item) sheet.inventory of 
    Just _ -> true
    Nothing -> false

canEquip :: Item -> CharacterSheet -> Boolean
canEquip item (CharacterSheet sheet) = characterLevel >= req
  where 
    characterLevel = unLevel (levelof sheet.xp)
    req = (unItem item).levelRequirement


equip :: Item -> CharacterSheet -> Either String CharacterSheet
equip item sheet | not $ hasItem item sheet = Left "Player doest not have that item in inventory"
equip item sheet | not $ canEquip item sheet = Left "Player does not meet the level requirement for that item"
equip item (CharacterSheet sheet) = do
  let equipped = sheet.equipped
  case item of 
    Helmet item' -> 
      Right $ CharacterSheet $ sheet { equipped = equipped { helmet = Just $ Helmet item' } }
    Chest item' -> 
      Right $ CharacterSheet $ sheet { equipped = equipped { chest = Just $ Chest item' } }
    Hands item' -> 
      Right $ CharacterSheet $ sheet { equipped = equipped { hands = Just $ Hands item' } }
    Leggs item' -> 
      Right $ CharacterSheet $ sheet { equipped = equipped { leggs = Just $ Leggs item' } }
    Feet item' -> 
      Right $ CharacterSheet $ sheet { equipped = equipped { feet = Just $ Feet item' } }