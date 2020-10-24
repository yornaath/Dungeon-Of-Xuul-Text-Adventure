module Data.Item where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Stats (Stats, emptyStats)

type ItemProps = {
  name :: String,
  description :: String,
  stats :: Stats,
  levelRequirement :: Int
}

data Item = 
    Helmet ItemProps
  | Chest ItemProps
  | Hands ItemProps
  | Leggs ItemProps
  | Feet ItemProps

derive instance genericItem:: Generic Item _

instance showItem :: Show Item where
  show = genericShow

instance eqItem :: Eq Item where
  eq = genericEq

instance encodeJsonItem :: EncodeJson Item where
  encodeJson a = genericEncodeJson a

instance decodeJsonItem :: DecodeJson Item where
  decodeJson a = genericDecodeJson a

unItem :: Item -> ItemProps
unItem item = 
  case item of 
    Helmet item' -> item'
    Chest item' -> item'
    Hands item' -> item'
    Leggs item' -> item'
    Feet item' -> item'