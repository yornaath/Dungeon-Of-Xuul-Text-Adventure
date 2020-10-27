module Game.Loop.CharacterCreation where
  
import Prelude

import Game.Data.Character (mkCharacterSheet)
import Game.Data.Die (d20, tossDies)
import Game.Data.Experience (Experience(..))
import Data.Int (fromString)
import Data.List (List(..), foldl, (:))
import Game.Data.Maps (dungeonOfXul)
import Data.Maybe (Maybe(..))
import Game.Data.Role (Role(..))
import Game.Data.Stats (Stats, mkStats)
import Data.String (toLower)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Game.Engine (Engine, prompt, log)
import Game.GameState (GameState(..))
import Game.Loop.CharacterCreation.CreatingCharacterState (CreatingCharacterState)
import Game.Loop.Playing.PlayingState (startGame)
import Static.Text as StaticText

characterCreation :: CreatingCharacterState -> Array String -> Engine GameState
characterCreation state input = do
  log StaticText.characterCreationHeader
  case input of 
    [":exit"] -> do
      pure (MainMenu)
    _ -> do
      creationform state
  
creationform :: CreatingCharacterState -> Engine GameState
creationform state = do

  log "Name: \n"
  name <- prompt

  log $ "Name: " <> name <> "\n"

  role <- chooseRole state

  log $ "Chose class: " <> show role <> "\n"

  stats <- allocateStats state

  let characterSheet = mkCharacterSheet name role stats (Experience 0)

  log "-----------------------------------"
  log $ show characterSheet
  log "----------------------------------- \n"
  
  log "Happy with this choice? (type yes to continue, no to start over): \n"
  confirmed <- prompt

  if (toLower confirmed) == "y" || (toLower confirmed) == "yes" then 
    pure (Playing $ startGame characterSheet dungeonOfXul)
  else
    creationform state


chooseRole :: CreatingCharacterState -> Engine Role
chooseRole state = do
  log "Choose a class(Warrior, Thief or Mage): \n"
  roleInput <- prompt 
  case toLower roleInput of 
    "thief" ->    do pure Thief
    "warrior" ->  do pure Warrior
    "mage" ->     do pure Mage
    _ -> do
      log (roleInput <> " is not a recognized class. \n")
      chooseRole state


allocateStats :: CreatingCharacterState -> Engine Stats
allocateStats state = do
  
  rolledDies <- liftEffect $ sequence $ tossDies (d20 : d20 : d20 : Nil)

  let total = (foldl (+) 0 rolledDies)

  log "\nYou rolled 3x D20's for available stats:"
  log $ "Results: " <> (foldl (\acc d -> acc <> (show d) <> " ") "" rolledDies)
  log $ "Total: " <> (show total)

  log "Keep rolls? (yes to keep, no to re-roll):\n"
  confirmed <- prompt 

  if (toLower confirmed) == "n" || (toLower confirmed) == "no" then 
    allocateStats state
  else do
    (Tuple agi pointsLeft) <- pickStat "Agility" total
    (Tuple str pointsLeft') <- pickStat "Strength" pointsLeft
    (Tuple end pointsLeft'') <- pickStat "Endurance" pointsLeft'
    (Tuple wis pointsLeft''') <- pickStat "Wisdom" pointsLeft''
    (Tuple int pointsLeft'''') <- pickStat "Inteligence" pointsLeft'''
    pure (mkStats agi str end wis int)


pickStat :: String -> Int -> (Engine (Tuple Int Int))
pickStat statname total = do
  log ("Choose " <> statname <> " (points left: " <> show total <> "):\n ")
  numString <- prompt 
  let num = fromString numString
  case num of 
    Nothing -> do 
      log $ "Invalid Number \n."
      pickStat statname total
    (Just num') -> do
      let newTotal = total - num'
      if newTotal < 0 then do
        log $ "You dont have that may points left, (" <> (show total) <> ") available. \n"
        pickStat statname total
      else
        pure (Tuple num' (newTotal))