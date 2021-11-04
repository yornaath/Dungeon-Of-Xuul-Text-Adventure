module Game.Syntax.Spec where

import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)

import Prelude

data Expression =
    Load String
  | Save String
  | Action PlayerAction
  | Exit

derive instance genericExpression:: Generic Expression _

instance showExpression :: Show Expression where
  show = genericShow

instance eqExpression :: Eq Expression where
  eq = genericEq

data PlayerAction 
  = Idle 
  | Move String
  | Look
  | Inspect String
  | Take String
  | TakeItemFrom String String
  | Consume String
  | OpenCharacterSheet
  | Combat CombatTurn
  | TalkTo String
  | Dialogue DialogueTurn

derive instance genericPlayerAction:: Generic PlayerAction _

instance showPlayerAction :: Show PlayerAction where
  show = genericShow

instance eqPlayerAction :: Eq PlayerAction where
  eq = genericEq


data CombatTurn = 
    Attack String
  | Cast String
  | Defend

derive instance genericCombatTurn:: Generic CombatTurn _

instance showCombatTurn:: Show CombatTurn where
  show = genericShow

instance eqCombatTurn :: Eq CombatTurn where
  eq = genericEq


data DialogueTurn =
  Answer Int 

derive instance genericDialogueTurn:: Generic DialogueTurn _

instance showDialogueTurn:: Show DialogueTurn where
  show = genericShow

instance eqDialogueTurn :: Eq DialogueTurn where
  eq = genericEq