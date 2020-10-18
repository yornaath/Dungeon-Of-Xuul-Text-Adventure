module Lib.AffReadline where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (wrap)
import Data.String (split)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Node.ReadLine (Interface)
import Node.ReadLine as RL

question :: Interface -> String -> Aff String
question interface message = makeAff go
  where
    -- go :: (Either Error a -> Effect Unit) -> Effect Canceler
    go runAffFunction = nonCanceler <$
      RL.question message (runAffFunction <<< Right) interface


command :: Interface -> String -> Aff (Array String)
command interface prompt = do 
  answer <- question interface prompt
  let 
    command' :: Array String
    command' = (split (wrap " ")) answer
  pure command'

closeInterface :: Interface -> Effect Unit
closeInterface interface = do
  log "Now closing interface"
  RL.close interface
  log "Finished!"