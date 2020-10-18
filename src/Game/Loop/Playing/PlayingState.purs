module Game.Loop.Playing.PlayingState where

import Data.Character (CharacterSheet)
import Data.Location (Location)

type PlayingState = {
  turn :: Int,
  character :: CharacterSheet,
  location :: Location
}

startGame :: CharacterSheet -> Location -> PlayingState
startGame character location = {
  turn: 0,
  character,
  location
}