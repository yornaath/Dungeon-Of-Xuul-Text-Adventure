module Game.Data.Character where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.Foldable (find)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Int (floor, toNumber)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Game.Data.Character.GearSlot (GearSlot(..))
import Game.Data.Experience (Experience, Level(..), levelofExperience)
import Game.Data.Item.Equipment (Equipment(..), levelRequirementOf, statsOf)
import Game.Data.Role (Role)
import Game.Data.Stats (Endurance(..), Stats(..), emptyStats, mkStats, total)

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

type Equiped = M.Map GearSlot (Maybe Equipment)

derive instance genericCharacterSheet:: Generic CharacterSheet _

instance showCharacterSheet :: Show CharacterSheet where
  show (CharacterSheet c) =
    let 
      (Stats stats) = totalStats $ CharacterSheet c
      maxhp' = maxhp (CharacterSheet c)
      in
        "Name:          " <> c.name                               <> "\n" <>
        "Level:         " <> show (levelofExperience c.xp)        <> "\n" <>
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

instance eqCharacterSheet :: Eq CharacterSheet where
  eq a = genericEq a


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
    (Level l) = level characterSheet
    (Stats totalStats) = totalStats characterSheet
    (Endurance end) = totalStats.end
    hp' = (10 * l) * floor ((toNumber end) * 1.5)
  in
    hp'

level :: CharacterSheet -> Level
level (CharacterSheet sheet) = levelofExperience sheet.xp

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

-- How can I use alt to give different error strings
canEquip :: Equipment -> CharacterSheet -> Boolean
canEquip item (CharacterSheet sheet) = characterLevel >= req
  where
    characterLevel = levelofExperience sheet.xp
    req = levelRequirementOf item


type EquipError = String

equip :: CharacterSheet -> GearSlot -> Equipment -> Either EquipError CharacterSheet

equip character slot item | not $ canEquip item character = 
  Left $ "You do not meet the level requirement for that item. \n" <> 
         "You: " <> (show $ level character) <> "\n" <> 
         "Item: " <> (show $ levelRequirementOf item)

equip (CharacterSheet character) (Head) (Helmet item) = 
  Right $ CharacterSheet $ character { equipped = setItemInSlot Head character.equipped (Helmet item) }
equip (CharacterSheet character) (Chest) (ChestPlate item) = 
  Right $ CharacterSheet $ character { equipped = setItemInSlot Chest character.equipped (ChestPlate item) }
equip (CharacterSheet character) (Hands) (Gloves item) = 
  Right $ CharacterSheet $ character { equipped = setItemInSlot Hands character.equipped (Gloves item) }
equip (CharacterSheet character) (Leggs) (LeggGuards item) = 
  Right $ CharacterSheet $ character { equipped = setItemInSlot Leggs character.equipped (LeggGuards item) }
equip (CharacterSheet character) (Feet) (Shoes item) = 
  Right $ CharacterSheet $ character { equipped = setItemInSlot Feet character.equipped (Shoes item) }

equip (CharacterSheet character) (MainHand) item = case item of 
  LongSword item' -> do
    Right $ CharacterSheet $ character { equipped = setItemInSlot MainHand character.equipped (LongSword item') }
  GreatSword item' -> 
    Right $ CharacterSheet $ character { equipped = setItemInSlot MainHand character.equipped (GreatSword item') # unEquipSlot OffHand }
  Dagger item' -> 
    Right $ CharacterSheet $ character { equipped = setItemInSlot MainHand character.equipped (Dagger item') }
  Bow item' -> 
    Right $ CharacterSheet $ character { equipped = setItemInSlot MainHand character.equipped (Bow item') # unEquipSlot OffHand }
  Staff item' -> 
    Right $ CharacterSheet $ character { equipped = setItemInSlot MainHand character.equipped (Staff item') # unEquipSlot OffHand }
  _ -> 
    Left $ "Cannot equip " <> show item <> " in slot: " <> show MainHand

equip (CharacterSheet character) (OffHand) item = case item of 
  LongSword item' -> do
    let 
      equipped = setItemInSlot OffHand character.equipped (LongSword item')
      equipped' = case M.lookup MainHand character.equipped  of 
        Just (Just (GreatSword a)) -> unEquipSlot MainHand equipped
        Just (Just (Bow a)) -> unEquipSlot MainHand equipped
        Just (Just (Staff a)) -> unEquipSlot MainHand equipped
        _ -> equipped
    Right $ CharacterSheet $ character { equipped = equipped' }    
  Dagger item' -> do
    let 
      equipped = setItemInSlot OffHand character.equipped (Dagger item')
      equipped' = case M.lookup MainHand character.equipped  of 
        Just (Just (GreatSword a)) -> unEquipSlot MainHand equipped
        Just (Just (Bow a)) -> unEquipSlot MainHand equipped
        Just (Just (Staff a)) -> unEquipSlot MainHand equipped
        _ -> equipped
    Right $ CharacterSheet $ character { equipped = equipped' }    
  _ -> 
    Left $ "Cannot equip " <> show item <> " in slot: " <> show MainHand


equip character slot item = 
  Left $ "Cannot equip " <> show item <> " in slot: " <> show slot

setItemInSlot :: GearSlot -> Equiped -> Equipment -> Equiped
setItemInSlot slot equiped equipmentConstructor = 
  M.update (\_ -> Just $ Just (equipmentConstructor)) slot equiped

unEquipSlot :: GearSlot -> Equiped -> Equiped
unEquipSlot slot equiped =
  M.update (\_ -> Just $ Nothing) slot equiped
