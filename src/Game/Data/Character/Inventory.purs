module Game.Data.Character.Inventory (
  Inventory,
  Equiped,
  GearSlot(..),
  InventoryItem,
  equip,
  equippedStats
) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Array (deleteAt, findIndex, fromFoldable, snoc)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Game.Data.Item.Equipment (Equipment(..), statsOf)
import Game.Data.Stats (Stats)


--
-- Inventory
--

type Inventory = {
  equiped :: Equiped,
  carrying :: Array InventoryItem
}

equip :: GearSlot -> Equipment -> Inventory -> Either String Inventory
equip slot equipment inventory = do 
  (Tuple takenItem carrying) <- takeEquipmentFromInventory equipment inventory
  (Tuple equiped returnedItem) <- equip'' slot equipment inventory.equiped
  let 
    carrying' = case returnedItem of
      Nothing -> carrying
      Just item' -> snoc carrying $ InventoryEquipment item'
  pure { equiped, carrying: carrying' }

takeEquipmentFromInventory :: Equipment -> Inventory -> Either String (Tuple Equipment (Array InventoryItem))
takeEquipmentFromInventory equipment inventory = 
  let itemIndex = findIndex ((==) (InventoryEquipment equipment)) inventory.carrying
  in case itemIndex of 
    Nothing -> Left "Equipment not found in inventory"
    Just index -> do
      let carrying = inventory.carrying
      let carryingWithItemRemoved = fromMaybe carrying (deleteAt index carrying )
      Right $ Tuple equipment carryingWithItemRemoved

equip'' :: GearSlot -> Equipment -> Equiped -> Either String (Tuple Equiped (Maybe Equipment)) 
equip'' Head (Helmet item) equiped = 
  Right $ Tuple (setItemInSlot Head equiped (Helmet item)) Nothing
equip'' Chest (ChestPlate item) equiped = 
  Right $ Tuple (setItemInSlot Chest equiped (ChestPlate item)) Nothing
equip'' Hands (Gloves item) equiped = 
  Right $ Tuple (setItemInSlot Hands equiped (Gloves item)) Nothing
equip'' Leggs (LeggGuards item) equiped = 
  Right $ Tuple (setItemInSlot Leggs equiped (LeggGuards item)) Nothing
equip'' Feet (Shoes item) equiped = 
  Right $ Tuple (setItemInSlot Feet equiped (Shoes item)) Nothing

equip'' MainHand item equiped = 
  case item of 
    LongSword item' -> do
      Right $ Tuple (setItemInSlot MainHand equiped (LongSword item')) Nothing
    GreatSword item' -> do
      let equiped' = setItemInSlot MainHand equiped (GreatSword item')
      let Tuple unslottedItem equiped'' = unEquipSlot OffHand equiped'
      Right $ Tuple equiped'' unslottedItem
    Dagger item' -> do
      Right $ Tuple (setItemInSlot MainHand equiped (Dagger item')) Nothing
    Bow item' -> do
      let equiped' = setItemInSlot MainHand equiped (Bow item')
      let Tuple unslottedItem equiped'' = unEquipSlot OffHand equiped'
      Right $ Tuple equiped'' unslottedItem
    Staff item' -> do
      let equiped' = setItemInSlot MainHand equiped (Staff item')
      let Tuple unslottedItem equiped'' = unEquipSlot OffHand equiped'
      Right $ Tuple equiped'' unslottedItem
    _ -> do
      Left $ "Cannot equip " <> show item <> " in slot: " <> show MainHand

equip'' OffHand item equiped = case item of 
  LongSword item' -> do
    let 
      equiped' = setItemInSlot OffHand equiped (LongSword item')
      mainHand = M.lookup MainHand equiped'
      Tuple unslottedItem equiped'' = case mainHand of 
        Just (GreatSword a) -> unEquipSlot MainHand equiped
        Just (Bow a) -> unEquipSlot MainHand equiped
        Just (Staff a) -> unEquipSlot MainHand equiped
        _ -> Tuple Nothing equiped'
    Right $ Tuple equiped'' Nothing
  Dagger item' -> do
    let 
      equiped' = setItemInSlot OffHand equiped (Dagger item')
      mainHand = M.lookup MainHand equiped'
      Tuple unslottedItem equiped'' = case mainHand of 
        Just (GreatSword a) -> unEquipSlot MainHand equiped
        Just (Bow a) -> unEquipSlot MainHand equiped
        Just (Staff a) -> unEquipSlot MainHand equiped
        _ -> Tuple Nothing equiped'
    Right $ Tuple equiped'' Nothing
  Shield item' -> do
    let 
      equiped' = setItemInSlot OffHand equiped (Shield item')
      mainHand = M.lookup MainHand equiped'
      Tuple unslottedItem equiped'' = case mainHand of 
        Just (GreatSword a) -> unEquipSlot MainHand equiped
        Just (Bow a) -> unEquipSlot MainHand equiped
        Just (Staff a) -> unEquipSlot MainHand equiped
        _ -> Tuple Nothing equiped'
    Right $ Tuple equiped'' Nothing
  _ -> do
      Left $ "Cannot equip " <> show item <> " in slot: " <> show MainHand

equip'' _ _ _ = Left "Could not perform that inventory action."

--
-- InventoryItem
-- The types/catagories of inventory items
--

data InventoryItem = 
  InventoryEquipment Equipment

derive instance genericInventoryItem:: Generic InventoryItem _

instance eqInventoryItem :: Eq InventoryItem where
  eq = genericEq

instance showInventoryItem :: Show InventoryItem where
  show = genericShow

instance encodeJsonInventoryItem :: EncodeJson InventoryItem where
  encodeJson a = genericEncodeJson a

instance decodeJsonInventoryItem :: DecodeJson InventoryItem where
  decodeJson a = genericDecodeJson a



--
-- Equiped
-- Represesents the currently equiped gear.
--

type Equiped = M.Map GearSlot Equipment

setItemInSlot :: GearSlot -> Equiped -> Equipment -> Equiped
setItemInSlot slot' equiped equipmentConstructor = 
  M.update (\_ -> Just (equipmentConstructor)) slot' equiped

unEquipSlot :: GearSlot -> Equiped -> Tuple (Maybe Equipment) Equiped
unEquipSlot slot' equiped = Tuple unequipedItem newEquipment where
  unequipedItem = M.lookup slot' equiped
  newEquipment = M.update (\_ -> Nothing) slot' equiped

equippedStats :: Equiped -> Array Stats
equippedStats equiped = (\i -> statsOf i) <$> equipedItems where
  equipedItems = fromFoldable $ M.values equiped



--
-- GearSlot
-- Represesents a slot for Equipment, like head, hands, mainhand etc
--

data GearSlot
  = Head
  | Chest
  | Hands
  | Leggs
  | Feet
  | MainHand
  | OffHand 

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

