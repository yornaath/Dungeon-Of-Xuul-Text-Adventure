module Data.Item where

import Data.Stats (Stats, emptyStats)

data Item = Item {
  name :: String,
  description :: String,
  stats :: Stats
}

torch :: Item
torch = Item {
  name : "Torch",
  description: "can light up a room",
  stats: emptyStats
}
