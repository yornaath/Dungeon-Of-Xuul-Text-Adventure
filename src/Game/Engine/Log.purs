module Engine.Log where

import Effect (Effect)
import Queue as Q

type LogLine = String

type Log = Q.Queue (read :: Q.READ, write :: Q.WRITE) LogLine

empty :: Effect Log 
empty = Q.new