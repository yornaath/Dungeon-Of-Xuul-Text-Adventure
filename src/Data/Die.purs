module Data.Die where

import Prelude

import Data.List (List(..), (:))
import Effect (Effect)
import Effect.Random (randomInt)
  
data Die = Die Int

instance showDie :: Show Die where
  show (Die n) = "d" <> show n

instance eqDie :: Eq Die where
  eq (Die a) (Die b) = a == b

d20 :: Die
d20 = Die 20

d6 :: Die
d6 = Die 6

allDies :: List Die
allDies = (d6 : d20 : Nil)

rollDie :: Die -> Effect Int
rollDie (Die n) = randomInt 1 n

tossDies :: List Die -> List (Effect Int)
tossDies ds = map rollDie ds

sumDies :: List (Effect Int) -> Effect Int
sumDies ds = go 0 ds where
  go :: Int -> List (Effect Int) -> Effect Int
  go acc Nil = do pure acc
  go acc (d : ds') = do
    n <- d
    go (acc + n) ds'
  