module Data.Location where
  
import Data.List (List)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)

data Location = Location {
  name :: String,
  description :: String,
  exits :: List Exit
}

derive instance genericLocation :: Generic Location _

instance encodeJsonLocation :: EncodeJson Location where
  encodeJson a = genericEncodeJson a

instance decodeJsonLocation :: DecodeJson Location where
  decodeJson a = genericDecodeJson a

newtype Exit = Exit {
  name :: String,
  description :: String,
  location :: Location
}

derive instance genericExit :: Generic Exit _

instance encodeJsonExit :: EncodeJson Exit where
  encodeJson a = genericEncodeJson a

instance decodeJsonExit :: DecodeJson Exit where
  decodeJson a = genericDecodeJson a
