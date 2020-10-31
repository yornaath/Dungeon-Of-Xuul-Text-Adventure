module Game.Data.Character.Inventory (
  Inventory,
  Equiped,
  GearSlot(..),
  InventoryItem(..),
  equip,
  equippedStats
) where

import Prelude

import Control.Monad.Trans.Class (lift)
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
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Game.Data.Item.Equipment (Equipment(..), statsOf)
import Game.Data.Stats (Stats, emptyStats)


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
        unslottedItem = getInSlot OffHand equiped'
      Right $ Tuple equiped' unslottedItem
    Dagger item' -> do
      Right $ Tuple (setItemInSlot MainHand equiped item) Nothing
    Bow item' -> do
      let equiped' = setItemInSlot MainHand equiped item
      let unslottedItem  = getInSlot OffHand equiped'
      Right $ Tuple equiped' unslottedItem
    Staff item' -> do
      let equiped' = setItemInSlot MainHand equiped item
      let unslottedItem  = getInSlot OffHand equiped'
      Right $ Tuple equiped' unslottedItem
    _ -> do
      Left $ "Cannot equip " <> show item <> " in slot: " <> show MainHand

_equip OffHand item equiped = case item of 
  LongSword item' -> do
    let 
      equiped' = setItemInSlot OffHand equiped (LongSword item')
      mainHand = getInSlot MainHand equiped
      unslottedItem = case mainHand of 
        Just (GreatSword a) -> getInSlot MainHand equiped
        Just (Bow a) -> getInSlot MainHand equiped
        Just (Staff a) -> getInSlot MainHand equiped
        _ -> Nothing
    Right $ Tuple equiped' unslottedItem
  Dagger item' -> do
    let 
      equiped' = setItemInSlot OffHand equiped (Dagger item')
      mainHand = getInSlot MainHand equiped'
      unslottedItem = case mainHand of 
        Just (GreatSword a) -> getInSlot MainHand equiped
        Just (Bow a) -> getInSlot MainHand equiped
        Just (Staff a) -> getInSlot MainHand equiped
        _ -> Nothing
    Right $ Tuple equiped' unslottedItem
  Shield item' -> do
    let 
      equiped' = setItemInSlot OffHand equiped (Shield item')
      mainHand = getInSlot MainHand equiped'
      unslottedItem = case mainHand of 
        Just (GreatSword a) -> getInSlot MainHand equiped
        Just (Bow a) -> getInSlot MainHand equiped
        Just (Staff a) -> getInSlot MainHand equiped
        _ -> Nothing
    Right $ Tuple equiped' unslottedItem
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



--
-- Equiped
-- Represesents the currently equiped gear.
--

type Equiped = {
  head :: Maybe Equipment,
  chest :: Maybe Equipment,
  hands :: Maybe Equipment,
  leggs :: Maybe Equipment,
  feet :: Maybe Equipment,
  mainHand :: Maybe Equipment,
  offHand :: Maybe Equipment
}

setItemInSlot :: GearSlot -> Equiped -> Equipment -> Equiped
setItemInSlot slot equiped equipment = case slot of 
  Head -> equiped { head = Just equipment }
  Chest -> equiped { chest = Just equipment }
  Hands -> equiped { hands = Just equipment }
  Leggs -> equiped { leggs = Just equipment }
  Feet -> equiped { feet = Just equipment }
  MainHand -> equiped { mainHand = Just equipment }
  OffHand  -> equiped { offHand = Just equipment }

getInSlot :: GearSlot -> Equiped -> Maybe Equipment
getInSlot slot equiped = case slot of 
  Head -> equiped.head
  Chest -> equiped.chest
  Hands -> equiped.hands
  Leggs -> equiped.leggs
  Feet -> equiped.feet
  MainHand -> equiped.mainHand
  OffHand  -> equiped.offHand

equippedStats :: Equiped -> Array Stats
equippedStats equiped =
  let 
    mapper = (\item -> case item of 
      Nothing -> emptyStats
      Just item' -> statsOf item'
    )
    equipedItems = [
      equiped.head,
      equiped.chest,
      equiped.hands,
      equiped.leggs,
      equiped.feet,
      equiped.mainHand,
      equiped.offHand
    ]
  in
    mapper <$> equipedItems

-- type Equiped = M.Map GearSlot Equipment

-- setItemInSlot :: GearSlot -> Equiped -> Equipment -> Equiped
-- setItemInSlot slot equiped equipment = 
--   M.insert slot equipment equiped

-- getInSlot :: GearSlot -> Equiped -> Maybe Equipment
-- getInSlot slot equiped = M.lookup slot equiped

-- equippedStats :: Equiped -> Array Stats
-- equippedStats equiped = (\i -> statsOf i) <$> equipedItems where
--   equipedItems = fromFoldable $ M.values equiped



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

