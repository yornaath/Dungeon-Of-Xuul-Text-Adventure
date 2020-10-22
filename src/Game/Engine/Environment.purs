module Engine.Environment where

import Engine.Input (Input)
import Engine.Log (Log)
import Queue (Queue, READ, WRITE)

type Environment = {
  input :: Input,
  log :: Log
}

