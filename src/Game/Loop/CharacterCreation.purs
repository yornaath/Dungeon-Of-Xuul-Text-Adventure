module Game.Loop.CharacterCreation where
  
import Prelude

import Control.Monad.Reader (ask)
import Data.Die (d20, tossDies)
import Data.List (List(..), fold, foldl, (:))
import Data.Role (Role(..))
import Data.Stats (Stats, mkStats)
import Data.String (toLower)
import Data.Traversable (sequence)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Game.Game (Game, liftGame)
import Game.GameState (CreatingCharacterState, GameState(..))
import Lib.AffReadline (question)
import Node.ReadLine (Interface)
import Static.Text (characterCreationHeader)

characterCreation :: CreatingCharacterState -> Array String -> Game GameState
characterCreation state input = do
  log characterCreationHeader
  creationform state
  

creationform :: CreatingCharacterState -> Game GameState
creationform state = do

  { interface } <- ask

  name <- liftAff $ interface # question "Character name: "
  role <- liftGame $ chooseRole state

  rolledDies <- liftEffect $ sequence $ tossDies (d20 : d20 : d20 : Nil)

  log "\nYou rolled 3x D20's for available stats:"
  log $ "Results: " <> (foldl (\acc d -> acc <> (show d) <> " ") "" rolledDies)
  log $ "Total: " <> show (foldl (+) 0 rolledDies)

  stats <- allocateStats state rolledDies

  log "-----------------------------------"
  log $ "Name: " <> name <> "\nRole: " <> show role
  log "----------------------------------- \n"
  
  confirmed <- liftAff $ interface # question "Happy with this choice? "

  if (toLower confirmed) == "y" || (toLower confirmed) == "yes" then 
    pure (CreatingCharacter state)
  else
    creationform state

chooseRole :: CreatingCharacterState -> Game Role
chooseRole state = do
  { interface } <- ask
  roleInput <- liftAff $ interface # question "Choose a class(Warrior, Thief or Mage): "
  case toLower roleInput of 
    "thief" -> do pure Thief
    "warrior" -> do pure Warrior
    "mage" -> do pure Mage
    _ -> do
      log (roleInput <> " is not a recognized class.")
      chooseRole state

allocateStats :: CreatingCharacterState -> List Int -> Game Stats
allocateStats state rolledDies = do
  { interface } <- ask
  allocation <- liftAff $ interface # question "Allocate"
  pure (mkStats 0 0 0 0 0)