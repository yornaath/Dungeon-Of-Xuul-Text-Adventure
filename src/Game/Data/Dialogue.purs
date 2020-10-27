module Game.Data.Dialogue where

import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)

type Dialogue = Map Int ChoicePoint

type ChoicePoint = (Tuple String (Array Reply))

type Reply = 
  { text :: String, 
    next :: Maybe Int
  }
