module Game.Data.Item.Equipment where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Game.Data.Experience (Level)
import Game.Data.Stats (Stats)


data Equipment = 
    Helmet EquipmentProps
  | ChestPlate EquipmentProps
  | Gloves EquipmentProps
  | LeggGuards EquipmentProps
  | Shoes EquipmentProps
  | LongSword WeaponProps
  | GreatSword WeaponProps
  | Dagger WeaponProps
  | Bow WeaponProps
  | Staff WeaponProps
  | Shield ShieldProps

type BaseProps =
  ( name :: String
  , description :: String
  , stats :: Stats
  , levelRequirement :: Level 
  )
 
type EquipmentProps = { | BaseProps }

type WeaponProps = { damage :: Int | BaseProps } 
  
type ShieldProps = { block :: Int | BaseProps } 

derive instance genericEquipment:: Generic Equipment _

instance showEquipment :: Show Equipment where
  show = genericShow

instance eqEquipment :: Eq Equipment where
  eq = genericEq

instance encodeJsonEquipment :: EncodeJson Equipment where
  encodeJson a = genericEncodeJson a

instance decodeJsonEquipment :: DecodeJson Equipment where
  decodeJson a = genericDecodeJson a

levelRequirementOf :: Equipment -> Level
levelRequirementOf item = 
  case item of 
    Helmet armor' -> armor'.levelRequirement
    ChestPlate armor' -> armor'.levelRequirement
    Gloves armor' -> armor'.levelRequirement
    LeggGuards armor' -> armor'.levelRequirement
    Shoes armor' -> armor'.levelRequirement
    LongSword weapon' -> weapon'.levelRequirement
    GreatSword weapon' -> weapon'.levelRequirement
    Dagger weapon' -> weapon'.levelRequirement
    Bow weapon' -> weapon'.levelRequirement
    Staff weapon' -> weapon'.levelRequirement
    Shield shield' -> shield'.levelRequirement

statsOf :: Equipment -> Stats
statsOf item = 
  case item of 
    Helmet armor' -> armor'.stats
    ChestPlate armor' -> armor'.stats
    Gloves armor' -> armor'.stats
    LeggGuards armor' -> armor'.stats
    Shoes armor' -> armor'.stats
    LongSword weapon' -> weapon'.stats
    GreatSword weapon' -> weapon'.stats
    Dagger weapon' -> weapon'.stats
    Bow weapon' -> weapon'.stats
    Staff weapon' -> weapon'.stats
    Shield shield' -> shield'.stats

