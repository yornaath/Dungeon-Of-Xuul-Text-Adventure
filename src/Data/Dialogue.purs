module Data.Dialogue where

import Prelude

import Data.Array (mapWithIndex)
import Data.Foldable (foldl)
import Data.Map as M
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))

type Dialogue = M.Map Int ChoicePoint

type ChoicePoint = (Tuple String (Array Reply))

type Reply = 
  { text :: String, 
    next :: Maybe Int
  }

renderChoicePoint :: ChoicePoint -> String
renderChoicePoint (Tuple text replies) = 
  "\n“" <> text <> "”\n\n" <>
  foldl (\acc reply -> acc <> renderReply reply) "" (mapWithIndex (\index reply -> (Tuple index reply)) replies)

renderReply :: Tuple Int Reply -> String
renderReply (Tuple index reply) =
  (show (index + 1)) <> ". " <> reply.text <> "\n"
