module Game.Syntax.Spec where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data PlayerAction = 
  Move String

derive instance genericPlayerAction:: Generic PlayerAction _

instance showPlayerAction :: Show PlayerAction where
  show = genericShow


data Expression =
    Load String
  | Save String
  | Turn PlayerAction

derive instance genericExpression:: Generic Expression _

instance showExpression :: Show Expression where
  show = genericShow
