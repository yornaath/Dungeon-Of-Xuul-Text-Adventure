module Game.Engine where
  
import Prelude

import Control.Monad.RWS (asks)
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Data.List as L
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Game.GameEnvironment (GameEnvironment)
import Type.Equality (class TypeEquals, from)

type Log = L.List String

newtype Engine a = Engine (ReaderT GameEnvironment Aff a)

runEngine:: GameEnvironment -> Engine ~> Aff
runEngine env (Engine m) = runReaderT m env

derive newtype instance functorGame :: Functor Engine
derive newtype instance applyGame :: Apply Engine
derive newtype instance applicativeGame :: Applicative Engine
derive newtype instance bindGame :: Bind Engine
derive newtype instance monadGame :: Monad Engine
derive newtype instance monadEffectGame :: MonadEffect Engine
derive newtype instance monadAffGame :: MonadAff Engine

class Monad m <= EngineCapability m where
  liftEngine :: forall a. Engine a -> m a
  
instance monadEngine :: TypeEquals e GameEnvironment => MonadAsk e Engine where
  ask = Engine $ asks from