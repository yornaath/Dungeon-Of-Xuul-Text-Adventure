module Game.Loop.CharacterCreation where
  
import Prelude

import Control.Monad.Reader (ask)
import Data.Character (mkCharacterSheet)
import Data.Die (d20, tossDies)
import Data.Experience (Experience(..))
import Data.Int (fromString)
import Data.List (List(..), foldl, (:))
import Data.Maps (dungeonOfXul)
import Data.Maybe (Maybe(..))
import Data.Role (Role(..))
import Data.Stats (Agility(..), Stats, mkStats)
import Data.String (toLower)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Game.Game (Game, liftGame)
import Game.GameState (CreatingCharacterState, GameState(..))
import Lib.AffReadline (question)
import Static.Text (characterCreationHeader)

characterCreation :: CreatingCharacterState -> Array String -> Game GameState
characterCreation state input = do
  log characterCreationHeader
  creationform state
  

creationform :: CreatingCharacterState -> Game GameState
creationform state = do

  { interface } <- ask

  name <- liftAff $ interface # question "Character name: "
  role <- chooseRole state
  stats <- allocateStats state

  let characterSheet = mkCharacterSheet name role stats (Experience 0)

  log "-----------------------------------"
  log $ show characterSheet
  log "----------------------------------- \n"
  
  confirmed <- liftAff $ interface # question "Happy with this choice? (type yes to continue, no to start over) "

  if (toLower confirmed) == "y" || (toLower confirmed) == "yes" then 
    pure (Playing { character: characterSheet, location: dungeonOfXul })
  else
    creationform state

chooseRole :: CreatingCharacterState -> Game Role
chooseRole state = do
  { interface } <- ask
  roleInput <- liftAff $ interface # question "Choose a class(Warrior, Thief or Mage): "
  case toLower roleInput of 
    "thief" ->    do pure Thief
    "warrior" ->  do pure Warrior
    "mage" ->     do pure Mage
    _ -> do
      log (roleInput <> " is not a recognized class.")
      chooseRole state

allocateStats :: CreatingCharacterState -> Game Stats
allocateStats state = do
  { interface } <- ask
  
  rolledDies <- liftEffect $ sequence $ tossDies (d20 : d20 : d20 : Nil)

  let total = (foldl (+) 0 rolledDies)

  log "\nYou rolled 3x D20's for available stats:"
  log $ "Results: " <> (foldl (\acc d -> acc <> (show d) <> " ") "" rolledDies)
  log $ "Total: " <> (show total)

  confirmed <- liftAff $ interface # question "Keep rolls? (yes to keep, no to re-roll): "

  if (toLower confirmed) == "n" || (toLower confirmed) == "no" then 
    allocateStats state
  else do
    (Tuple agi pointsLeft) <- pickStat "agility" total
    (Tuple str pointsLeft') <- pickStat "strength" pointsLeft
    (Tuple end pointsLeft'') <- pickStat "endurance" pointsLeft'
    (Tuple wis pointsLeft''') <- pickStat "wisdomw" pointsLeft''
    (Tuple int pointsLeft'''') <- pickStat "inteligence" pointsLeft'''
    pure (mkStats agi str end wis int)


pickStat :: String -> Int -> (Game (Tuple Int Int))
pickStat statname total = do
  { interface } <- ask
  numString <- liftAff $ interface # question ("Choose " <> statname <> " (" <> show total <> "): ")
  let num = fromString numString
  case num of 
    Nothing -> do 
      log $ "Invalid Number"
      pickStat statname total
    (Just num') -> do
      let newTotal = total - num'
      if newTotal < 0 then do
        log $ "You dont have that may points left, (" <> (show total) <> ") available"
        pickStat statname total
      else
        pure (Tuple num' (newTotal))