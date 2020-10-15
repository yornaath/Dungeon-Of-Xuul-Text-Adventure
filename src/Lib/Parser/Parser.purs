module Lib.Parser where

import Prelude

import Data.Array (foldl, fromFoldable, toUnfoldable)
import Data.Char (toCharCode)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (logShow)

  
data JsonValue
  = JsonNull
  | JsonBool Boolean
  | JsonNumber Number
  | JsonString String
  | JsonArray (Array JsonValue)
  | JsonObject (Array (Tuple String JsonValue))


data Parser a = Parser (String -> Maybe (Tuple String a))

instance functorParser :: Functor Parser where
  map f (Parser a) = Parser b
    where
      b str = do
        (Tuple xs x) <- a str
        Just (Tuple xs (f x))

instance applyParser :: Apply Parser where
  apply (Parser p1) (Parser p2) = Parser \input -> do
    (Tuple input' f) <- p1 input
    (Tuple input'' a) <- p2 input'
    Just (Tuple input'' (f a))

instance applicativeParser :: Applicative Parser where
  pure x = Parser (\str -> Just (Tuple str x))

parse :: forall a. String -> Parser a -> Maybe (Tuple String a)
parse str (Parser f) = f str

l :: forall a. List a -> List a
l a = a

charP :: Char -> Parser Char
charP x = Parser (toCharArray >>> toUnfoldable >>> f)
  where
    f (y:ys)
      | y == x = Just (Tuple (foldl (\acc s -> acc <> fromCharArray [s]) "" ys) x)
      | otherwise = Nothing
    f Nil = Nothing

-- stringP :: String -> Parser String
-- stringP str = sequence (map charP str)

main :: Effect Unit
main = do
  let charA = charP 'a'
  let charAInt = map (\c -> toCharCode c) charA
  logShow $ parse "aasd" charAInt