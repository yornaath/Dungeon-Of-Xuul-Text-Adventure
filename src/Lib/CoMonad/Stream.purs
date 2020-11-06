module Lib.CoMonad.Stream where

import Prelude

import Control.Comonad (class Comonad, class Extend, duplicate, extend, extract, (=>=))
import Data.Lazy (Lazy, force, defer)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Prim.Row (class Cons)

data Stream a = Cons a (Lazy (Stream a))

-- instance lazyStream :: Lazy (Stream a) where
--   defer s = s unit

instance functorStream :: Functor Stream where
  map f (Cons a as) = Cons (f a) (defer \u -> map f (force as))

instance extendStream :: Extend Stream where
  extend f s@(Cons a as) = Cons (f s) (defer \u -> (extend f (force as)))

instance comonadStream :: Comonad Stream where
  extract (Cons a _) = a

next :: forall a. Stream a -> Stream a
next (Cons a as) = force as

sumWithNext :: Stream Int -> Int
sumWithNext (Cons a as) = a + b where 
  (Cons b _) = force as

incrementStream :: Int -> Stream Int
incrementStream start = Cons start (defer \u -> incrementStream (start + 1))

summedIncrement :: Stream Int -> Stream Int
summedIncrement ints = extend sumWithNext ints

main :: Effect Unit
main = do
  let streamA = summedIncrement $ incrementStream 1
  logShow $ extract streamA
  let streamB = next streamA
  logShow $ extract streamB
  let streamC = next streamB
  logShow $ extract streamC

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
  