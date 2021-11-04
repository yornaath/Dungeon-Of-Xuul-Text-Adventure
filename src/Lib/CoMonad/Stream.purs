module Lib.CoMonad.Stream where

import Prelude

import Control.Comonad (class Comonad, class Extend, duplicate, extend, extract, (<<=))
import Control.Comonad.Store (Store, StoreT(..))
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (State)
import Control.Monad.State.Class (get, put)
import Data.Lazy (Lazy, force, defer)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Effect.Ref as Ref
import Lib.AffReadline (question)
import Node.ReadLine as RL

class Pairing f g | f -> g, g -> f where
  pair:: forall a b c. (a -> b -> c) -> f a -> g b -> c

move :: forall w m a b. (Comonad w) => (Pairing m w) => w a -> m b -> w a
move space movement = pair (\_ newSpace -> newSpace) movement (duplicate space)


data Stream a = Cons a (Lazy (Stream a))

instance functorStream :: Functor Stream where
  map f (Cons a as) = Cons (f a) (defer \u -> map f (force as))

instance extendStream :: Extend Stream where
  extend f s@(Cons a as) = Cons (f s) (defer \u -> (extend f (force as)))

instance comonadStream :: Comonad Stream where
  extract (Cons a _) = a



data Sequence a = End a | Next (Sequence a)

instance functorSequence :: Functor Sequence where
  map f (End a) = End $ f a
  map f (Next sa) = map f sa

instance applySequence :: (Functor Sequence) => Apply Sequence where
  apply (End f) (End a) = End $ f a
  apply f b = f <*> b

instance applicativeSequence :: (Apply Sequence) => Applicative Sequence where
  pure a = End a

instance bindSequence :: (Applicative Sequence) => Bind Sequence where
  bind (End a) f = f a
  bind (Next as) f = Next (as >>= f)

instance monadSequence :: (Bind Sequence) => Monad Sequence

instance pairingSequence :: Pairing Sequence Stream where
  pair f (End a) (Cons b _) = f a b
  pair f (Next seq) (Cons _ stream) = pair f seq (force stream)


unfoldStream :: forall a s. s -> (s -> Tuple a s) -> Stream a
unfoldStream initialState next = Cons a (defer \u -> (unfoldStream nextState next)) where
  Tuple a nextState = next initialState

incrementStream :: Int -> Stream Int
incrementStream start = unfoldStream start \s -> Tuple s (s + 1)  

sumWithNext :: Stream Int -> Int
sumWithNext (Cons a as) = a + b where 
  (Cons b _) = force as

summedIncrement :: Stream Int -> Stream Int
summedIncrement ints = sumWithNext <<= ints



type UI base m a = (m -> base) -> a

type ComponentT base w m a = w (UI base m a)

data ConsoleC = ConsoleC { action:: String -> Effect Unit, text:: String }

counterComponent :: ComponentT (Effect Unit) Stream (Sequence Unit) ConsoleC
counterComponent = unfoldStream 0 (\state -> Tuple (render state) (state + 1)) where
  render:: Int -> UI (Effect Unit) (Sequence Unit) ConsoleC
  render state = \send ->
    ConsoleC
      { text: show state,
        action: \input -> send (Next $ End unit)
      }

explore' :: forall w m. (Comonad w) => (Pairing m w) => ComponentT (Effect Unit) w (m Unit) ConsoleC -> Effect Unit
explore' component = do
  ref <- Ref.new component
  interface <- RL.createConsoleInterface RL.noCompletion
  let 
    loop = do
      space <- Ref.read ref
      let send action = Ref.write (move space action) ref
      let ConsoleC {text, action} = extract space send
      log text
      RL.question ">" (\answer -> do
        liftEffect $ action answer
        loop
      ) interface
  
  loop
    

main :: Effect Unit
main = do
  -- let stream = incrementStream 1
  -- let offset = move stream (Next $ Next $ Next $ End unit)
  -- logShow $  extract offset
  explore' counterComponent






-- data Env e a = Env e a

-- instance functorEnv :: Functor (Env a) where
--   map f (Env e a) = Env e (f a)

-- instance extendEnv :: Extend (Env a) where 
--   extend f w@(Env e a) = Env e (f w)

-- instance envComonad :: Comonad (Env a) where
--   extract (Env e a) = a

-- ask ::forall e a. Env e a -> e
-- ask (Env e _) = e

-- asks :: forall e e' a. (e -> e') -> (Env e a) -> e'
-- asks f (Env e a) = f e


-- type Config = {
--   n :: Int
-- }

-- add :: Env Config Int-> Int
-- add w = (extract w) + n where n = (ask w).n

-- times :: Env Config Int -> Int
-- times w = (extract w) * n where n = (ask w).n

-- pipe :: Env Config Int -> Int
-- pipe = add =>= times

-- main :: Effect Unit
-- main = do
--   logShow $ pipe $ Env {n : 2} 2
  