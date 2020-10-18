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
import Data.Stats (Stats, mkStats)
import Data.String (toLower)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Game.Engine (Engine)
import Game.GameState (GameState(..))
import Game.Loop.Playing.PlayingState (startGame)
import Lib.AffReadline (question)
import Static.Text as StaticText
import Game.Loop.CharacterCreation.CreatingCharacterState (CreatingCharacterState)

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

  { interface } <- ask

  name <- liftAff $ question interface "Character name: "

  role <- chooseRole state
  stats <- allocateStats state

  let characterSheet = mkCharacterSheet name role stats (Experience 0)

  log "-----------------------------------"
  log $ show characterSheet
  log "----------------------------------- \n"
  
  confirmed <- liftAff $ question interface "Happy with this choice? (type yes to continue, no to start over): "

  if (toLower confirmed) == "y" || (toLower confirmed) == "yes" then 
    pure (Playing $ startGame characterSheet dungeonOfXul)
  else
    creationform state


chooseRole :: CreatingCharacterState -> Engine Role
chooseRole state = do
  { interface } <- ask
  roleInput <- liftAff $ question interface "Choose a class(Warrior, Thief or Mage): "
  case toLower roleInput of 
    "thief" ->    do pure Thief
    "warrior" ->  do pure Warrior
    "mage" ->     do pure Mage
    _ -> do
      log (roleInput <> " is not a recognized class.")
      chooseRole state


allocateStats :: CreatingCharacterState -> Engine Stats
allocateStats state = do
  { interface } <- ask
  
  rolledDies <- liftEffect $ sequence $ tossDies (d20 : d20 : d20 : Nil)

  let total = (foldl (+) 0 rolledDies)

  log "\nYou rolled 3x D20's for available stats:"
  log $ "Results: " <> (foldl (\acc d -> acc <> (show d) <> " ") "" rolledDies)
  log $ "Total: " <> (show total)

  confirmed <- liftAff $ question interface "Keep rolls? (yes to keep, no to re-roll): "

  if (toLower confirmed) == "n" || (toLower confirmed) == "no" then 
    allocateStats state
  else do
    (Tuple agi pointsLeft) <- pickStat "agility" total
    (Tuple str pointsLeft') <- pickStat "strength" pointsLeft
    (Tuple end pointsLeft'') <- pickStat "endurance" pointsLeft'
    (Tuple wis pointsLeft''') <- pickStat "wisdomw" pointsLeft''
    (Tuple int pointsLeft'''') <- pickStat "inteligence" pointsLeft'''
    pure (mkStats agi str end wis int)


pickStat :: String -> Int -> (Engine (Tuple Int Int))
pickStat statname total = do
  { interface } <- ask
  numString <- liftAff $ question interface ("Choose " <> statname <> " (" <> show total <> "): ")
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