module Game.Data.Experience where
  
import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Int (floor, toNumber)
import Data.List (List, findIndex, length, (..))
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype)

newtype Experience = Experience Int

derive instance newtypeExperience :: Newtype Experience _
derive newtype instance semiringExperience :: Semiring Experience
derive newtype instance encodeJsonExperience :: EncodeJson Experience
derive newtype instance decodeJsonExperience :: DecodeJson Experience
derive newtype instance showExperience :: Show Experience
derive newtype instance eqExperience :: Eq Experience
derive newtype instance ordExperience :: Ord Experience


newtype Level = Level Int

derive instance newtypeLevel :: Newtype Level _
derive newtype instance semiringLevel :: Semiring Level
derive newtype instance encodeJsonLevel :: EncodeJson Level
derive newtype instance decodeJsonLevel :: DecodeJson Level
derive newtype instance showLevel :: Show Level
derive newtype instance eqLevel :: Eq Level
derive newtype instance ordLevel :: Ord Level

unLevel:: Level -> Int
unLevel (Level l) = l

levels :: List Int
levels = map (\b -> floor $ (toNumber b) * (1.0 + (toNumber b) * 2.0)) (1..60)

levelofExperience :: Experience -> Level
levelofExperience (Experience xp) =
  let 
    levels' = levels
    level' = (+) 1 $ fromMaybe ((-) (length levels') 1) (findIndex (\minxp -> minxp > xp) levels')
  in
    Level level'


