module Engine.Input where

import Effect (Effect)
import Queue as Q

type InputAction = String

type Input = Q.Queue (read :: Q.READ, write :: Q.WRITE) InputAction

empty :: Effect Input 
empty = Q.new