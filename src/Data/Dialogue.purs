module Data.Dialogue where

import Prelude

import Data.Array (mapWithIndex)
import Data.Foldable (foldl)
import Data.Map as M
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))

type Dialogue = M.Map Int ChoicePoint

type ChoicePoint = (Tuple String (Array Reply))

type Reply = 
  { text :: String, 
    next :: Maybe Int
  }
