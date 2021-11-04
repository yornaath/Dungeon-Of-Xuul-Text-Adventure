module Game.Data.Location where
  
import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Data.List (List)

data Location = Location {
  name :: String,
  description :: String,
  exits :: List Exit
}

derive instance genericLocation :: Generic Location _

instance showLocation :: Show Location where
  show loc = genericShow loc

instance eqLocation :: Eq Location where
  eq loc = genericEq loc

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

instance eqExit :: Eq Exit where
  eq exit = genericEq exit

derive instance genericExit :: Generic Exit _

instance encodeJsonExit :: EncodeJson Exit where
  encodeJson a = genericEncodeJson a

instance decodeJsonExit :: DecodeJson Exit where
  decodeJson a = genericDecodeJson a
