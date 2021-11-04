module Game.Data.Role where
  
import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..))
import Data.String (toLower)

data Role =
    Thief
  | Warrior
  | Mage

roleFromString :: String -> Maybe Role
roleFromString str = 
  case toLower str of
    "thief" -> Just Thief
    "warrior" -> Just Warrior
    "mage" -> Just Mage
    _ -> Nothing

derive instance genericRole:: Generic Role _

instance showRole :: Show Role where
  show = genericShow

instance eqRole :: Eq Role where
  eq a = genericEq a

instance encodeJsonRole :: EncodeJson Role where
  encodeJson a = genericEncodeJson a

instance decodeJsonRole :: DecodeJson Role where
  decodeJson a = genericDecodeJson a