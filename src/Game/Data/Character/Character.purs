module Game.Data.Character where

import Prelude

import Data.Int (floor, toNumber)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Game.Data.Character.Inventory (Inventory)
import Game.Data.Character.Inventory.Equiped (GearSlot(..), equipedFromFoldable, equippedStats)
import Game.Data.Experience (Experience, Level(..), levelofExperience)
import Game.Data.Item.Equipment (Equipment(..), levelRequirementOf)
import Game.Data.Role (Role)
import Game.Data.Stats (Endurance(..), Stat(..), Stats(..), mkStats, total)

type CharacterSheet =
  { name        :: String,
    stats       :: Stats,
    role        :: Role,
    xp          :: Experience,
    hp          :: Int,
    inventory   :: Inventory
  }

showCharacter :: CharacterSheet -> String
showCharacter c =
  let 
    (Stats stats) = totalStats $ c
    maxhp' = maxhp c
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


-- _equipped:: forall a r. Lens' { equipped :: a | r } a
-- _equipped= prop (SProxy :: SProxy "equipped")

-- _inventory :: forall a r. Lens' { inventory :: a | r } a
-- _inventory = prop (SProxy :: SProxy "inventory")

-- _equipment :: forall a r. Lens' { equipment :: a | r } a
-- _equipment = prop (SProxy :: SProxy "equipment")

-- _inventoryEquipment :: Traversal' CharacterSheet (Array Equipment)
-- _inventoryEquipment = _inventory <<< _equipment

mkCharacterSheet :: String -> Role -> Stats -> Experience -> CharacterSheet
mkCharacterSheet name role stats xp =
  let 
    sheet = {
      name, role, stats, xp, hp: 0,
      inventory: {
        equiped: equipedFromFoldable [
          Tuple Head $ Helmet { name: "Howler", description: "Nasty looking thing", stats: mkStats 2 4 0 0 0, levelRequirement: Level 0}
        ],
        carrying: []
      }
    }
    hp = maxhp sheet
  in
    sheet { hp = hp }


maxhp :: CharacterSheet -> Int
maxhp characterSheet = 
  let
    sheet' = characterSheet
    (Stats stats) = sheet'.stats
    (Level l) = level characterSheet
    (Stats totalStats) = totalStats characterSheet
    (Stat end) = totalStats.end
    hp' = (10 * l) * floor ((toNumber end) * 1.5)
  in
    hp'

level :: CharacterSheet -> Level
level sheet = levelofExperience sheet.xp

totalStats :: CharacterSheet -> Stats
totalStats (sheet) = total allStats
  where 
    allStats = [sheet.stats] <> (equippedStats sheet.inventory.equiped)

-- How can I use alt to give different error strings
canEquip :: Equipment -> CharacterSheet -> Boolean
canEquip item sheet = characterLevel >= req
  where
    characterLevel = levelofExperience sheet.xp
    req = levelRequirementOf item


