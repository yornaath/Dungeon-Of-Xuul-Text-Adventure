module Game.Syntax.Spec where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)

data PlayerAction = 
    Move String
  | Take String
  | TakeItemFrom String String
  | Look
  | Inspect String

derive instance genericPlayerAction:: Generic PlayerAction _

instance showPlayerAction :: Show PlayerAction where
  show = genericShow

instance eqPlayerAction :: Eq PlayerAction where
  eq = genericEq


data Expression =
    Load String
  | Save String
  | Turn PlayerAction

derive instance genericExpression:: Generic Expression _

instance showExpression :: Show Expression where
  show = genericShow

instance eqExpression :: Eq Expression where
  eq = genericEq
