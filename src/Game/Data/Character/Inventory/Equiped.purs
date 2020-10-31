module Game.Data.Character.Inventory.Equiped  where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Foldable (class Foldable, foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Game.Data.Item.Equipment (Equipment, statsOf)
import Game.Data.Stats (Stats, emptyStats)


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

equipedFromFoldable :: forall f. Foldable f => f (Tuple GearSlot Equipment) -> Equiped
equipedFromFoldable slots = foldl f empty slots where
  empty = { head: Nothing, chest: Nothing, hands: Nothing, leggs: Nothing, feet: Nothing, mainHand: Nothing, offHand: Nothing}
  f = (\equiped (Tuple slot item) -> setItemInSlot slot equiped item)

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

unslot :: GearSlot -> Equiped -> Tuple (Maybe Equipment) Equiped
unslot slot equiped = case slot of 
  Head -> Tuple equiped.head equiped { head = Nothing }
  Chest -> Tuple equiped.chest equiped { chest = Nothing }
  Hands -> Tuple equiped.hands equiped { hands = Nothing }
  Leggs -> Tuple equiped.leggs equiped { leggs = Nothing }
  Feet -> Tuple equiped.feet equiped { feet = Nothing }
  MainHand -> Tuple equiped.mainHand equiped { mainHand = Nothing }
  OffHand  -> Tuple equiped.offHand equiped { offHand = Nothing }

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

