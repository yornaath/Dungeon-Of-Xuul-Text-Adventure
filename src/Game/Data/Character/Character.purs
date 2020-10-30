module Game.Data.Character where

import Prelude

import Data.Int (floor, toNumber)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Game.Data.Character.Inventory (GearSlot(..), Inventory, equippedStats)
import Game.Data.Experience (Experience, Level(..), levelofExperience)
import Game.Data.Item.Equipment (Equipment(..), levelRequirementOf)
import Game.Data.Role (Role)
import Game.Data.Stats (Endurance(..), Stats(..), mkStats, total)

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
        equiped: M.fromFoldable [
          Tuple (Head) $ Just $ Helmet { name: "Howler", description: "Nasty looking thing", stats: mkStats 2 4 0 0 0, levelRequirement: Level 0},
          Tuple (Chest) Nothing,
          Tuple (Hands) Nothing,
          Tuple (Leggs) Nothing,
          Tuple (Feet) Nothing,
          Tuple (MainHand) Nothing,
          Tuple (OffHand) Nothing
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
    (Endurance end) = totalStats.end
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



-- equip :: CharacterSheet -> GearSlot -> Equipment -> V EquipErrors CharacterSheet
-- equip character slot equipment = 
--   let

--     doEquip :: CharacterSheet -> GearSlot -> Equipment -> V EquipError (Tuple (Array Equipment) Equiped)
--     doEquip character' (Head) (Helmet item) = 
--       V $ Right (Tuple character'.inventory.equipment (setItemInSlot Head character'.equipped (Helmet item)))
--     doEquip character' (Chest) (ChestPlate item) = 
--       V $ Right (Tuple character'.inventory.equipment (setItemInSlot Chest character'.equipped (ChestPlate item)))
--     doEquip character' (Hands) (Gloves item) = 
--       V $ Right (Tuple character'.inventory.equipment (setItemInSlot Hands character'.equipped (Gloves item)))
--     doEquip character' (Leggs) (LeggGuards item) = 
--       V $ Right (Tuple character'.inventory.equipment (setItemInSlot Leggs character'.equipped (LeggGuards item)))
--     doEquip character' (Feet) (Shoes item) = 
--       V $ Right (Tuple character'.inventory.equipment (setItemInSlot Feet character'.equipped (Shoes item)))

--     -- doEquip character (MainHand) item = 
--     --   case item of 
--     --     LongSword item' -> do
--     --       V $ Right $ Tuple Nothing (setItemInSlot MainHand character.equipped (LongSword item'))
--     --     GreatSword item' -> 
--     --       V $ Right $ CharacterSheet $ character { equipped = setItemInSlot MainHand character.equipped (GreatSword item') # unEquipSlot OffHand }
--     --     Dagger item' -> 
--     --       V $ Right $ Tuple Nothing (setItemInSlot MainHand character.equipped (Dagger item'))
--     --     Bow item' -> 
--     --       V $ Right $ CharacterSheet $ character { equipped = setItemInSlot MainHand character.equipped (Bow item') # unEquipSlot OffHand }
--     --     Staff item' -> 
--     --       V $ Right $ CharacterSheet $ character { equipped = setItemInSlot MainHand character.equipped (Staff item') # unEquipSlot OffHand }
--     --     _ -> 
--     --       V $ Left $ "Cannot equip " <> show item <> " in slot: " <> show MainHand

--     doEquip character' slot' item = 
--       V $ Left $ "Cannot equip " <> show item <> " in slot: " <> show slot

--   in do
--     let Tuple (inventoryEquipment) character' = takeEquipmentFromInventory character equipment
--     case inventoryEquipment of 
--       Nothing -> do
--         V $ Left $ ["You dont have that item in your inventory"]
--       Just item' -> do
--         let Tuple (newInventoryEquipment) newEquiped = doEquip character' slot item'
--         let characterWithNewEquipment = (set _equipped newEquiped character')
--         let characterWithItemPutBackIntoInventory = (set _inventoryEquipment newInventoryEquipment character')
--         V $ Right character

-- -- equip character slot item | not $ canEquip item character = 
-- --   Left $ "You do not meet the level requirement for that item. \n" <> 
-- --          "You: " <> (show $ level character) <> "\n" <> 
-- --          "Item: " <> (show $ levelRequirementOf item)

-- -- equip (CharacterSheet character) (Head) (Helmet item) = 
-- --   V $ Right $ CharacterSheet $ character { equipped = setItemInSlot Head character.equipped (Helmet item) }
-- -- equip (CharacterSheet character) (Chest) (ChestPlate item) = 
-- --   V $ Right $ CharacterSheet $ character { equipped = setItemInSlot Chest character.equipped (ChestPlate item) }
-- -- equip (CharacterSheet character) (Hands) (Gloves item) = 
-- --   V $ Right $ CharacterSheet $ character { equipped = setItemInSlot Hands character.equipped (Gloves item) }
-- -- equip (CharacterSheet character) (Leggs) (LeggGuards item) = 
-- --   V $ Right $ CharacterSheet $ character { equipped = setItemInSlot Leggs character.equipped (LeggGuards item) }
-- -- equip (CharacterSheet character) (Feet) (Shoes item) = 
-- --   V $ Right $ CharacterSheet $ character { equipped = setItemInSlot Feet character.equipped (Shoes item) }

-- -- equip (CharacterSheet character) (MainHand) item = case item of 
-- --   LongSword item' -> do
-- --     V $ Right $ CharacterSheet $ character { equipped = setItemInSlot MainHand character.equipped (LongSword item') }
-- --   GreatSword item' -> 
-- --     V $ Right $ CharacterSheet $ character { equipped = setItemInSlot MainHand character.equipped (GreatSword item') # unEquipSlot OffHand }
-- --   Dagger item' -> 
-- --     V $ Right $ CharacterSheet $ character { equipped = setItemInSlot MainHand character.equipped (Dagger item') }
-- --   Bow item' -> 
-- --     V $ Right $ CharacterSheet $ character { equipped = setItemInSlot MainHand character.equipped (Bow item') # unEquipSlot OffHand }
-- --   Staff item' -> 
-- --     V $ Right $ CharacterSheet $ character { equipped = setItemInSlot MainHand character.equipped (Staff item') # unEquipSlot OffHand }
-- --   _ -> 
-- --     V $ Left [ "Cannot equip " <> show item <> " in slot: " <> show MainHand]

-- -- equip (CharacterSheet character) (OffHand) item = case item of 
-- --   LongSword item' -> do
-- --     let 
-- --       equipped = setItemInSlot OffHand character.equipped (LongSword item')
-- --       equipped' = case M.lookup MainHand character.equipped  of 
-- --         Just (Just (GreatSword a)) -> unEquipSlot MainHand equipped
-- --         Just (Just (Bow a)) -> unEquipSlot MainHand equipped
-- --         Just (Just (Staff a)) -> unEquipSlot MainHand equipped
-- --         _ -> equipped
-- --     V $ Right $ CharacterSheet $ character { equipped = equipped' }    
-- --   Dagger item' -> do
-- --     let 
-- --       equipped = setItemInSlot OffHand character.equipped (Dagger item')
-- --       equipped' = case M.lookup MainHand character.equipped  of 
-- --         Just (Just (GreatSword a)) -> unEquipSlot MainHand equipped
-- --         Just (Just (Bow a)) -> unEquipSlot MainHand equipped
-- --         Just (Just (Staff a)) -> unEquipSlot MainHand equipped
-- --         _ -> equipped
-- --     V $ Right $ CharacterSheet $ character { equipped = equipped' }    
-- --   _ -> 
-- --     V $ Left ["Cannot equip " <> show item <> " in slot: " <> show MainHand]


-- -- equip character slot item = 
-- --   V $ Left ["Cannot equip " <> show item <> " in slot: " <> show slot]


