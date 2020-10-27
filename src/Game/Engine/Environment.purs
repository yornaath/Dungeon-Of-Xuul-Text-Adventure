module Engine.Environment where

import Engine.Input (Input)
import Engine.Log (Log)

type Environment = {
  input :: Input,
  log :: Log
}

