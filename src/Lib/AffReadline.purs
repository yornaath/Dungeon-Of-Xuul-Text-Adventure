module Lib.AffReadline where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Class.Console (log)
import Node.ReadLine (Interface)
import Node.ReadLine as RL

question :: String -> Interface -> Aff String
question message interface = makeAff go
  where
    -- go :: (Either Error a -> Effect Unit) -> Effect Canceler
    go runAffFunction = nonCanceler <$
      RL.question message (runAffFunction <<< Right) interface

closeInterface :: Interface -> Effect Unit
closeInterface interface = do
  log "Now closing interface"
  RL.close interface
  log "Finished!"