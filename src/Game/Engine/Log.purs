module Engine.Log where

import Prelude

import Effect (Effect)
import Halogen.HTML as HH
import Queue as Q

type LogLine  = HH.HTML Void Void

type Log = Q.Queue (read :: Q.READ, write :: Q.WRITE) LogLine

empty :: Effect Log 
empty = Q.new