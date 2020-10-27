module Game.Data.Location where
  
import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)

data Location = Location {
  name :: String,
  description :: String,
  exits :: List Exit
}

derive instance genericLocation :: Generic Location _

instance showLocation :: Show Location where
  show loc = genericShow loc

instance encodeJsonLocation :: EncodeJson Location where
  encodeJson a = genericEncodeJson a

instance decodeJsonLocation :: DecodeJson Location where
  decodeJson a = genericDecodeJson a

newtype Exit = Exit {
  name :: String,
  description :: String,
  location :: Location
}

derive newtype instance showExit :: Show Exit

derive instance genericExit :: Generic Exit _

instance encodeJsonExit :: EncodeJson Exit where
  encodeJson a = genericEncodeJson a

instance decodeJsonExit :: DecodeJson Exit where
  decodeJson a = genericDecodeJson a
