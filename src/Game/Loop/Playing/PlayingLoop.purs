module Game.Loop.Playing.PlayingLoop where

import Game.Loop.Playing.Dialogue.DialogueLoop
import Prelude

import Data.Either (Either(..))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String (split)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Engine.SaveGames (saveGame, loadGame)
import Game.Data.Dialogue (Dialogue)
import Game.Data.Location (Location(..))
import Game.Engine (Engine, liftEngine, log, prompt)
import Game.GameState (GameState(..))
import Game.Loop.Playing.PlayingState (ExplorationState, PlayingState(..))
import Game.Syntax.Parser (expressionParser)
import Game.Syntax.Spec (Expression(..), PlayerAction(..))
import Lib.Parser (runParser)

playing :: PlayingState -> PlayerAction -> Engine GameState
playing state action = do
  case state of 
    Exploration exploringState -> do 
      explorationLoop exploringState action
    CombatMode _ -> do
        log "[Combat not yet supported]"
        playing state Idle
    DialogueMode _ -> do
        log "[Dialogue not yet supported]"
        playing state Idle

explorationLoop :: ExplorationState -> PlayerAction -> Engine GameState
explorationLoop state action = 
  case action of
    Idle -> do 
      input' <- prompt
      case runParser expressionParser input' of 
        Left error -> do 
          liftEffect $ Console.log $ show error
          log "I dont understand that command."
          explorationLoop state Idle
        Right (Tuple action' _) -> do
          case action' of 
            Save saveName -> do
              saved <- liftAff $ saveGame saveName (Playing $ Exploration state)
              log "Game saved."
              explorationLoop state Idle
            Load saveName -> do 
              loadedState <- liftAff $ loadGame saveName
              pure loadedState
            Exit -> do
              pure (MainMenu)
            (Action playerAction) -> do
              explorationLoop state playerAction

    Look -> do
      let (Location loc) = state.location
      log $ loc.description <> "\n"
      explorationLoop state Idle

    OpenCharacterSheet -> do
      log $ show $ state.character
      explorationLoop state Idle

    _ -> do
      log "I dont understand that command."
      explorationLoop state Idle
  -- case input of
  --   ["help"] -> do
  --     log ":save savename     -- Save a game by its name"
  --     log ":exit              -- Exit to main menu"
  --     log ":c                 -- Inspect character \n"
  --     playing state []
  --   [":exit"] -> do
  --     pure (MainMenu)
  --   [":save", save] -> do
  --     saved <- liftAff $ saveGame save (Playing state)
  --     playing state []
  --   [":c"] -> do 
  --     log $ show state.character
  --     playing state []
  --   ["talk"] -> do
  --     liftEngine $ dialogue state testDialogue 1
  --   ["look"] -> do
  --     let (Location loc) = state.location
  --     log $ loc.description <> "\n"
  --     playing state []
  --   [] -> do
  --     input' <- prompt
  --     let command = (split (wrap " ")) input'
  --     playing state command
  --   _ -> do


testDialogue :: Dialogue
testDialogue = M.fromFoldable [
  (Tuple 1 
    (Tuple "Howdy there partner, how are you?" [
      { text: "I am fine... How did you get here?", next: Just 2 },
      { text: "Kind of tired to be honest... But have to bear on right?", next: Just 3 }
    ])
  ),
  (Tuple 2
    (Tuple "I have been here for a million years son." [
      { text: "Well ok then....", next: Nothing }
    ])
  ),
  (Tuple 3
    (Tuple "Right you are. Carry on." [
      { text: "«carry on»", next: Nothing }
    ])
  )
]