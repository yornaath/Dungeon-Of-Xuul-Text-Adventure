module Game.Syntax.Spec where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)

data Expression =
    Load String
  | Save String
  | Action PlayerAction

derive instance genericExpression:: Generic Expression _

instance showExpression :: Show Expression where
  show = genericShow

instance eqExpression :: Eq Expression where
  eq = genericEq

data PlayerAction = 
    Move String
  | Look
  | Inspect String
  | Take String
  | TakeItemFrom String String
  | Combat CombatTurn
  | Consume String

derive instance genericPlayerAction:: Generic PlayerAction _

instance showPlayerAction :: Show PlayerAction where
  show = genericShow

instance eqPlayerAction :: Eq PlayerAction where
  eq = genericEq

data CombatTurn = 
    Attack String
  | Cast String

derive instance genericCombatTurn:: Generic CombatTurn _

instance showCombatTurn:: Show CombatTurn where
  show = genericShow

instance eqCombatTurn :: Eq CombatTurn where
  eq = genericEq