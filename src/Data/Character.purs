module Data.Character where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.Experience (Experience, Level(..), levelof, unLevel)
import Data.Foldable (find, foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (floor, toNumber)
import Data.Item.Equipment (Equipment(..), levelRequirementOf, statsOf)
import Data.List ((:))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Role (Role)
import Data.Stats (Endurance(..), Stats(..), emptyStats, mkStats, total)
import Data.Tuple (Tuple(..))

data CharacterSheet = CharacterSheet 
  { name        :: String,
    stats       :: Stats,
    role        :: Role,
    xp          :: Experience,
    hp          :: Int,
    inventory   :: {
      equipment :: Array Equipment
    },
    equipped    :: Equiped
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


data GearSlot
  = Head
  | Chest
  | Hands
  | Leggs
  | Feet
  | MainHand
  | OffHand 

type Equiped = M.Map GearSlot (Maybe Equipment)

derive instance genericGearSlot:: Generic GearSlot _

instance eqGearSlot :: Eq GearSlot where
  eq = genericEq

instance ordGearSlot :: Ord GearSlot where
  compare = genericCompare

instance showGearSlot :: Show GearSlot where
  show = genericShow

instance encodeJsonGearSlot :: EncodeJson GearSlot where
  encodeJson a = genericEncodeJson a

instance decodeJsonGearSlot :: DecodeJson GearSlot where
  decodeJson a = genericDecodeJson a



mkCharacterSheet :: String -> Role -> Stats -> Experience -> CharacterSheet
mkCharacterSheet name role stats xp =
  let 
    sheet = {
      name, role, stats, xp, hp: 0,
      inventory: {
        equipment: []
      },
      equipped: M.fromFoldable [
        Tuple (Head) $ Just $ Helmet { name: "Howler", description: "Nasty looking thing", stats: mkStats 2 4 0 0 0, levelRequirement: Level 0},
        Tuple (Chest) Nothing,
        Tuple (Hands) Nothing,
        Tuple (Leggs) Nothing,
        Tuple (Feet) Nothing,
        Tuple (MainHand) Nothing,
        Tuple (OffHand) Nothing
      ]
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


equippedStats :: Equiped -> Array Stats
equippedStats equiped = itemStatsOrEmpty <$> equipedItems where
  itemStatsOrEmpty item = case item of  
    Just item' -> statsOf item' 
    Nothing -> emptyStats
  equipedItems = fromFoldable $ M.values equiped


totalStats :: CharacterSheet -> Stats
totalStats (CharacterSheet sheet) = total allStats
  where 
    allStats = [sheet.stats] <> (equippedStats sheet.equipped)


hasItem :: Equipment -> CharacterSheet -> Boolean
hasItem item (CharacterSheet sheet) = 
  case find (\item' -> item' == item) sheet.inventory.equipment of 
    Just _ -> true
    Nothing -> false


canEquip :: Equipment -> CharacterSheet -> Boolean
canEquip item (CharacterSheet sheet) = characterLevel >= req
  where
    characterLevel = levelof sheet.xp
    req = levelRequirementOf item

equip :: CharacterSheet -> GearSlot -> Equipment -> Either String CharacterSheet

equip (CharacterSheet character) (Head) (Helmet item) = 
  Right $ CharacterSheet $ character { equipped = M.update (\_ -> Just $ Just (Helmet item)) Head character.equipped }
equip (CharacterSheet character) (Chest) (ChestPlate item) = 
  Right $ CharacterSheet $ character { equipped = M.update (\_ -> Just $ Just (ChestPlate item)) Chest character.equipped }
equip (CharacterSheet character) (Hands) (Gloves item) = 
  Right $ CharacterSheet $ character { equipped = M.update (\_ -> Just $ Just (Gloves item)) Hands character.equipped }
equip (CharacterSheet character) (Leggs) (LeggGuards item) = 
  Right $ CharacterSheet $ character { equipped = M.update (\_ -> Just $ Just (LeggGuards item)) Leggs character.equipped }
equip (CharacterSheet character) (Feet) (Shoes item) = 
  Right $ CharacterSheet $ character { equipped = M.update (\_ -> Just $ Just (Shoes item)) Feet character.equipped }

equip (CharacterSheet character) (MainHand) item = case item of 
  (LongSword item') -> 
    Right $ CharacterSheet $ character { equipped = M.update (\_ -> Just $ Just (LongSword item')) MainHand character.equipped }
  -- GreatSword -> 

  -- Dagger -> 

  -- Bow -> 

  -- Staff -> 
  _ -> 
    Left $ "Cannot equip " <> show item <> " in slot: " <> show MainHand


equip character slot item = 
  Left $ "Cannot equip " <> show item <> " in slot: " <> show slot

unEquip :: CharacterSheet -> GearSlot -> CharacterSheet
unEquip (CharacterSheet character) slot =
  CharacterSheet $ character { equipped = M.update (\_ -> Just $ Nothing) slot character.equipped }
-- unEquip :: Item -> CharacterSheet -> Either String CharacterSheet
-- unEquip item (CharacterSheet sheet) =
--   case item of 
--     (sheet.equipped.helmet) -> Left "lol" 
--     -- "helmet"  ->
--     --   Right $ CharacterSheet $ sheet { equipped = equipped { helmet = Nothing } }
--     -- "chest"  -> 
--     --   Right $ CharacterSheet $ sheet { equipped = equipped { chest = Nothing } }
--     -- "hands"  -> 
--     --   Right $ CharacterSheet $ sheet { equipped = equipped { hands = Nothing } }
--     -- "leggs"  -> 
--     --   Right $ CharacterSheet $ sheet { equipped = equipped { leggs = Nothing } }
--     -- "feet"  -> 
--     --   Right $ CharacterSheet $ sheet { equipped = equipped { feet = Nothing } }
--     -- _ ->
--     --   Tuple Nothing sheet
