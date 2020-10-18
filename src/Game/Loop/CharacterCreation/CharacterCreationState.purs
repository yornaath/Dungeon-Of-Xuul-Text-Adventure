module Game.Loop.CharacterCreation.CreatingCharacterState where
  

import Data.Maybe (Maybe)
import Data.Role (Role)

type CreatingCharacterState = {
  name :: Maybe String,
  role :: Maybe Role
}