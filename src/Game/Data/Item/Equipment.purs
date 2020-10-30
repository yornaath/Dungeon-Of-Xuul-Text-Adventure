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

data Equipment
  = Head Head
  | Body Body
  | Hands Hands
  | Feet Feet
  | HandItem HandItem

data HandItem
  = MainHandItem MainHandItem
  | OffHandItem OffHandItem
  | EitherHandItem EitherHandItem
  | TwoHandedItem TwoHandedItem

derive instance genericHandItem :: Generic HandItem _

derive instance eqHandItem :: Eq HandItem

instance showHandItem :: Show HandItem where
  show = genericShow

instance encodeJsonHandItem :: EncodeJson HandItem where
  encodeJson = genericEncodeJson

instance decodeJsonHandItem :: DecodeJson HandItem where
  decodeJson = genericDecodeJson

data Head
  = Helmet EquipmentProps

derive instance genericHead :: Generic Head _

derive instance eqHead :: Eq Head

instance showHead :: Show Head where
  show = genericShow

instance encodeJsonHead :: EncodeJson Head where
  encodeJson = genericEncodeJson

instance decodeJsonHead :: DecodeJson Head where
  decodeJson = genericDecodeJson

data Body
  = ChestPlate EquipmentProps

derive instance genericBody :: Generic Body _

derive instance eqBody :: Eq Body

instance showBody :: Show Body where
  show = genericShow

instance encodeJsonBody :: EncodeJson Body where
  encodeJson = genericEncodeJson

instance decodeJsonBody :: DecodeJson Body where
  decodeJson = genericDecodeJson

data Hands
  = Gloves EquipmentProps

derive instance genericHands :: Generic Hands _

derive instance eqHands :: Eq Hands

instance showHands :: Show Hands where
  show = genericShow

instance encodeJsonHands :: EncodeJson Hands where
  encodeJson = genericEncodeJson

instance decodeJsonHands :: DecodeJson Hands where
  decodeJson = genericDecodeJson

data Feet
  = Shoes EquipmentProps

derive instance genericFeet :: Generic Feet _

derive instance eqFeet :: Eq Feet

instance showFeet :: Show Feet where
  show = genericShow

instance encodeJsonFeet :: EncodeJson Feet where
  encodeJson = genericEncodeJson

instance decodeJsonFeet :: DecodeJson Feet where
  decodeJson = genericDecodeJson

data MainHandItem
  = LongSword WeaponProps
  | Staff WeaponProps

derive instance genericMainHandItem :: Generic MainHandItem _

derive instance eqMainHandItem :: Eq MainHandItem

instance showMainHandItem :: Show MainHandItem where
  show = genericShow

instance encodeJsonMainHandItem :: EncodeJson MainHandItem where
  encodeJson = genericEncodeJson

instance decodeJsonMainHandItem :: DecodeJson MainHandItem where
  decodeJson = genericDecodeJson

data OffHandItem
  = Shield ShieldProps

derive instance genericOffHandItem :: Generic OffHandItem _

derive instance eqOffHandItem :: Eq OffHandItem

instance showOffHandItem :: Show OffHandItem where
  show = genericShow

instance encodeJsonOffHandItem :: EncodeJson OffHandItem where
  encodeJson = genericEncodeJson

instance decodeJsonOffHandItem :: DecodeJson OffHandItem where
  decodeJson = genericDecodeJson

data EitherHandItem
  = Dagger WeaponProps

derive instance genericEitherHandItem :: Generic EitherHandItem _

derive instance eqEitherHandItem :: Eq EitherHandItem

instance showEitherHandItem :: Show EitherHandItem where
  show = genericShow

instance encodeJsonEitherHandItem :: EncodeJson EitherHandItem where
  encodeJson = genericEncodeJson

instance decodeJsonEitherHandItem :: DecodeJson EitherHandItem where
  decodeJson = genericDecodeJson

data TwoHandedItem
  = GreatSword WeaponProps
  | Bow WeaponProps

derive instance genericTwoHandedItem :: Generic TwoHandedItem _

derive instance eqTwoHandedItem :: Eq TwoHandedItem

instance showTwoHandedItem :: Show TwoHandedItem where
  show = genericShow

instance encodeJsonTwoHandedItem :: EncodeJson TwoHandedItem where
  encodeJson = genericEncodeJson

instance decodeJsonTwoHandedItem :: DecodeJson TwoHandedItem where
  decodeJson = genericDecodeJson

type BaseProps
  = ( name :: String
    , description :: String
    , stats :: Stats
    , levelRequirement :: Level
    )

type EquipmentProps
  = { | BaseProps }

type WeaponProps
  = { damage :: Int | BaseProps }

type ShieldProps
  = { block :: Int | BaseProps }

derive instance genericEquipment :: Generic Equipment _

instance showEquipment :: Show Equipment where
  show = genericShow

instance eqEquipment :: Eq Equipment where
  eq = genericEq

instance encodeJsonEquipment :: EncodeJson Equipment where
  encodeJson a = genericEncodeJson a

instance decodeJsonEquipment :: DecodeJson Equipment where
  decodeJson a = genericDecodeJson a

levelRequirementOf :: Equipment -> Level
levelRequirementOf = case _ of
  Head h -> case h of
    Helmet r -> r.levelRequirement
  Body b -> case b of
    ChestPlate r -> r.levelRequirement
  Hands h -> case h of
    Gloves r -> r.levelRequirement
  Feet f -> case f of
    Shoes r -> r.levelRequirement
  HandItem h -> case h of
    MainHandItem m -> case m of
      LongSword r -> r.levelRequirement
      Staff r -> r.levelRequirement
    OffHandItem o -> case o of
      Shield r -> r.levelRequirement
    EitherHandItem e -> case e of
      Dagger r -> r.levelRequirement
    TwoHandedItem t -> case t of
      GreatSword r -> r.levelRequirement
      Bow r -> r.levelRequirement

statsOf :: Equipment -> Stats
statsOf item = case item of
  Head h -> case h of
    Helmet r -> r.stats
  Body b -> case b of
    ChestPlate r -> r.stats
  Hands h -> case h of
    Gloves r -> r.stats
  Feet f -> case f of
    Shoes r -> r.stats
  HandItem h -> case h of
    MainHandItem m -> case m of
      LongSword r -> r.stats
      Staff r -> r.stats
    OffHandItem o -> case o of
      Shield r -> r.stats
    EitherHandItem e -> case e of
      Dagger r -> r.stats
    TwoHandedItem t -> case t of
      GreatSword r -> r.stats
      Bow r -> r.stats
