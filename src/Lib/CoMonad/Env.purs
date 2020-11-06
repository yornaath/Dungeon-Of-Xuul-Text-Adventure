module Lib.CoMonad.Env where

import Prelude

import Control.Comonad (class Comonad, class Extend, extract, (=>=))
import Effect (Effect)
import Effect.Class.Console (logShow)

data Env e a = Env e a

instance functorEnv :: Functor (Env a) where
  map f (Env e a) = Env e (f a)

instance extendEnv :: Extend (Env a) where 
  extend f w@(Env e a) = Env e (f w)

instance envComonad :: Comonad (Env a) where
  extract (Env e a) = a

ask ::forall e a. Env e a -> e
ask (Env e _) = e

asks :: forall e e' a. (e -> e') -> (Env e a) -> e'
asks f (Env e a) = f e


type Config = {
  n :: Int
}

add :: Env Config Int-> Int
add w = (extract w) + n where n = (ask w).n

times :: Env Config Int -> Int
times w = (extract w) * n where n = (ask w).n

pipe :: Env Config Int -> Int
pipe = add =>= times

main :: Effect Unit
main = do
  logShow $ pipe $ Env {n : 2} 2
  