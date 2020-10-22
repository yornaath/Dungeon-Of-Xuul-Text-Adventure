module Play where
  
import Prelude

import Effect (Effect)
import Effect.Class.Console (log)


data Maybe a = Just a | Nothing

instance showMaybe :: Show a =>  Show (Maybe a) where
  show (Just x) = "Just " <> (show x)
  show Nothing = "Nothing"

instance maybeFunctor :: Functor Maybe where
  map f (Just x) = Just $ f x
  map f (Nothing) = Nothing

add1 :: Int -> Int
add1 n = n + 1

main :: Effect Unit 
main = do 
  let a = Just 1
  let b = add1 <$> a
  log $ show b
  pure unit