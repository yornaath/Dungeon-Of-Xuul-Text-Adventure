module Engine.Environment where

import Queue (Queue, READ, WRITE)

type Environment = {
  input :: Queue (read :: READ, write :: WRITE) String,
  log :: Queue (read :: READ, write :: WRITE) String
}

