module Game.Data.Character.Inventory (
  Inventory,
  InventoryItem(..),
  equip
) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Array (deleteAt, findIndex, snoc)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Game.Data.Character.Inventory.Equiped (Equiped, GearSlot(..), getInSlot, setItemInSlot, unslot)
import Game.Data.Item.Equipment (Equipment(..))


--
-- Inventory
--

type Inventory = {
  equiped :: Equiped,
  carrying :: Array InventoryItem
}


equip :: GearSlot -> Equipment -> Inventory -> Either String Inventory
equip slot equipment inventory = do 
  Tuple takenItem carrying <- takeEquipmentFromInventory equipment inventory
  Tuple equiped' unslottedItem <- _equip slot takenItem inventory.equiped
  let 
    carrying' = case unslottedItem of
      Nothing -> carrying
      Just item' -> snoc carrying (InventoryEquipment item')
  pure { equiped : equiped', carrying: carrying' }

_equip :: GearSlot -> Equipment -> Equiped -> Either String (Tuple Equiped (Maybe Equipment)) 
_equip Head (Helmet item) equiped = do
  let unslottedItem = getInSlot Head equiped
  Right $ Tuple (setItemInSlot Head equiped (Helmet item)) unslottedItem
_equip Chest (ChestPlate item) equiped = do
  let unslottedItem = getInSlot Chest equiped
  Right $ Tuple (setItemInSlot Chest equiped (ChestPlate item)) unslottedItem
_equip Hands (Gloves item) equiped = do
  let unslottedItem = getInSlot Hands equiped
  Right $ Tuple (setItemInSlot Hands equiped (Gloves item)) unslottedItem
_equip Leggs (LeggGuards item) equiped = do
  let unslottedItem = getInSlot Leggs equiped
  Right $ Tuple (setItemInSlot Leggs equiped (LeggGuards item)) unslottedItem
_equip Feet (Shoes item) equiped = do
  let unslottedItem = getInSlot Feet equiped
  Right $ Tuple (setItemInSlot Feet equiped (Shoes item)) unslottedItem

_equip MainHand item equiped = 
  case item of 
    LongSword item' -> do
      Right $ Tuple (setItemInSlot MainHand equiped item) Nothing
    GreatSword item' -> do
      let 
        equiped' = setItemInSlot MainHand equiped item
        Tuple unslottedItem equiped'' = unslot OffHand equiped'
      Right $ Tuple equiped'' unslottedItem
    Dagger item' -> do
      Right $ Tuple (setItemInSlot MainHand equiped item) Nothing
    Bow item' -> do
      let equiped' = setItemInSlot MainHand equiped item
      let Tuple unslottedItem equiped''  = unslot OffHand equiped'
      Right $ Tuple equiped'' unslottedItem
    Staff item' -> do
      let equiped' = setItemInSlot MainHand equiped item
      let Tuple unslottedItem equiped''  = unslot OffHand equiped'
      Right $ Tuple equiped'' unslottedItem
    _ -> do
      Left $ "Cannot equip " <> show item <> " in slot: " <> show MainHand

_equip OffHand item equiped = case item of 
  LongSword item' -> do
    let 
      equiped' = setItemInSlot OffHand equiped (LongSword item')
      mainHand = getInSlot MainHand equiped
      Tuple unslottedItem equiped'' = case mainHand of 
        Just (GreatSword a) -> unslot MainHand equiped'
        Just (Bow a) -> unslot MainHand equiped'
        Just (Staff a) -> unslot MainHand equiped'
        _ -> Tuple Nothing equiped'
    Right $ Tuple equiped'' unslottedItem
  Dagger item' -> do
    let 
      equiped' = setItemInSlot OffHand equiped (Dagger item')
      mainHand = getInSlot MainHand equiped'
      Tuple unslottedItem equiped'' = case mainHand of 
        Just (GreatSword a) -> unslot MainHand equiped'
        Just (Bow a) -> unslot MainHand equiped'
        Just (Staff a) -> unslot MainHand equiped'
        _ -> Tuple Nothing equiped'
    Right $ Tuple equiped'' unslottedItem
  Shield item' -> do
    let 
      equiped' = setItemInSlot OffHand equiped (Shield item')
      mainHand = getInSlot MainHand equiped'
      Tuple unslottedItem equiped'' = case mainHand of 
        Just (GreatSword a) -> unslot MainHand equiped'
        Just (Bow a) -> unslot MainHand equiped'
        Just (Staff a) -> unslot MainHand equiped'
        _ -> Tuple Nothing equiped'
    Right $ Tuple equiped'' unslottedItem
  _ -> do
      Left $ "Cannot equip " <> show item <> " in slot: " <> show MainHand

_equip _ _ _ = Left "Could not perform that inventory action."

takeEquipmentFromInventory :: Equipment -> Inventory -> Either String (Tuple Equipment (Array InventoryItem))
takeEquipmentFromInventory equipment inventory = 
  let itemIndex = findIndex ((==) (InventoryEquipment equipment)) inventory.carrying
  in case itemIndex of 
    Nothing -> Left "Equipment not found in inventory"
    Just index -> do
      let carrying = inventory.carrying
      let carryingWithItemRemoved = fromMaybe carrying (deleteAt index carrying )
      Right $ Tuple equipment carryingWithItemRemoved


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

