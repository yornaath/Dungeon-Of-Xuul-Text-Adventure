module Game.Engine where
  
import Prelude

import Control.Monad.RWS (asks)
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Data.Either (Either(..))
import Data.List as L
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, makeAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Engine.Environment (Environment)
import Queue as Q
import Type.Equality (class TypeEquals, from)

type Log = L.List String

newtype Engine a = Engine (ReaderT Environment Aff a)

runEngine:: Environment -> Engine ~> Aff
runEngine env (Engine m) = runReaderT m env

derive newtype instance functorGame :: Functor Engine
derive newtype instance applyGame :: Apply Engine
derive newtype instance applicativeGame :: Applicative Engine
derive newtype instance bindGame :: Bind Engine
derive newtype instance monadGame :: Monad Engine
derive newtype instance monadEffectGame :: MonadEffect Engine
derive newtype instance monadAffGame :: MonadAff Engine

class Monad m <= EngineM m where
  liftEngine :: forall a. Engine a -> m a
  prompt :: m String
  log :: String -> m Unit

instance monadAskEngine :: TypeEquals e Environment => MonadAsk e Engine where
  ask = Engine $ asks from

instance engineEngineM :: EngineM Engine where

  liftEngine = identity

  log str = do
    liftEffect $ Console.log str
    pure unit

  prompt = do
    inputQueue <- asks _.input
    let 
      takeNext = makeAff \cb -> do
        Q.once inputQueue \item -> do
          cb (Right item)
        mempty
    nextLine <- liftAff takeNext
    pure nextLine

