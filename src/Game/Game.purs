module Game.Game where
  
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

newtype Game a = Game (ReaderT GameEnvironment Aff a)

runGame:: GameEnvironment -> Game ~> Aff
runGame env (Game m) = runReaderT m env

derive newtype instance functorGame :: Functor Game
derive newtype instance applyGame :: Apply Game
derive newtype instance applicativeGame :: Applicative Game
derive newtype instance bindGame :: Bind Game
derive newtype instance monadGame :: Monad Game
derive newtype instance monadEffectGame :: MonadEffect Game
derive newtype instance monadAffGame :: MonadAff Game

class Monad m <= MonadGame m where
  liftGame :: forall a. Game a -> m a

instance monadGameGame :: MonadGame Game where
  liftGame = identity

instance monadAskAppM :: TypeEquals e GameEnvironment => MonadAsk e Game where
  ask = Game $ asks from