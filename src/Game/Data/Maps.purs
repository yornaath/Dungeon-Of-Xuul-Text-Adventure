module Game.Data.Maps where

import Data.List (List(..), (:))
import Game.Data.Location (Exit(..), Location(..))
  
dungeonOfXul :: Location
dungeonOfXul = Location {
  name : "Dungeon of Xul",
  description: "You are standing in front of a towering cliff.",
  exits: (
    Exit { name: "crack",
           description: "There is a crack in the wall, you could possibly squeeze through it.",
           location: entrance
    } : Nil
  )
}

entrance :: Location
entrance = Location {
  name : "Dungeon Entrance",
  description: "You are in a dark and dimly lit cave entrance.",
  exits: (Nil)
}