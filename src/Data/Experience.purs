module Data.Experience where
  
import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Int (floor, toNumber)
import Data.List (List, findIndex, length, (..))
import Data.Maybe (fromMaybe)

newtype Experience = Experience Int

derive instance genericExperience:: Generic Experience _

instance encodeJsonExperience :: EncodeJson Experience where
  encodeJson a = genericEncodeJson a

instance decodeJsonExperience :: DecodeJson Experience where
  decodeJson a = genericDecodeJson a

instance showExperience :: Show Experience where
  show (Experience xp) = "Experience " <> show xp

newtype Level = Level Int

unLevel:: Level -> Int
unLevel (Level l) = l

instance showLevel :: Show Level where
  show (Level l) = "Level " <> show l

levels :: List Int
levels = map (\b -> floor $ (toNumber b) * (1.0 + (toNumber b) * 2.0)) (1..60)

levelof :: Experience -> Level
levelof (Experience xp) =
  let 
    levels' = levels
    level' = (+) 1 $ fromMaybe ((-) (length levels') 1) (findIndex (\minxp -> minxp > xp) levels')
  in
    Level level'


