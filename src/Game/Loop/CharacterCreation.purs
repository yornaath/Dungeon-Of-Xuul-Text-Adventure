module Game.Loop.CharacterCreation where
  
import Prelude

import Control.Monad.Reader (ask)
import Data.Either (Either(..))
import Data.Role (Role(..))
import Data.String (toLower)
import Effect.Aff (Aff, makeAff)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log, logShow)
import Game.Game (Game(..), liftGame)
import Game.GameState (CreatingCharacterState, GameState(..))
import Lib.AffReadline (question)
import Node.ReadLine (Interface)

characterCreation :: CreatingCharacterState -> Array String -> Game GameState
characterCreation state input = do
  
  env <- ask

  logShow "CHARACTER CREATION"
  logShow "-------------------"

  creationform env.interface state
  

creationform :: Interface -> CreatingCharacterState -> Game GameState
creationform interface state = do
  name <- liftAff $ interface # question "Character name: "
  role <- liftGame $ chooseRole interface state

  logShow $ "Name: " <> name <> "\n Role: " <> show role <> " \n"
  
  confirmed <- liftAff $ interface # question "Happy with this choice? "

  if (toLower confirmed) == "y" || (toLower confirmed) == "yes" then 
    pure (CreatingCharacter state)
  else
    creationform interface state

chooseRole :: Interface -> CreatingCharacterState -> Game Role
chooseRole interface state = do
  roleInput <- liftAff $ interface # question "Choose a class(Warrior, Thief or Mage): "
  case toLower roleInput of 
    "thief" -> do pure Thief
    "warrior" -> do pure Warrior
    "mage" -> do pure Mage
    _ -> do
      log (roleInput <> " is not a recognized class.")
      chooseRole interface state