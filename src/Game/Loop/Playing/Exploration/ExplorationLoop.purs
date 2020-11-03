module Game.Loop.Playing.Exploration where

import Data.Either (Either(..))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Engine.SaveGames (saveGame, loadGame)
import Game.Data.Dialogue (Dialogue)
import Game.Data.Location (Location(..))
import Game.Engine (Engine, log, prompt)
import Game.GameState (GameState(..))
import Game.Loop.Playing.Dialogue.DialogueLoop (dialogueLoop)
import Game.Loop.Playing.PlayingState (ExplorationState, PlayingState(..))
import Game.Syntax.Parser (expressionParser)
import Game.Syntax.Spec (Expression(..), PlayerAction(..))
import Lib.Parser (runParser)
import Prelude (bind, discard, pure, show, ($), (<>))

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
    
    TalkTo _ -> do
      state' <- dialogueLoop state testDialogue 1
      explorationLoop state Idle

    OpenCharacterSheet -> do
      log $ show $ state.character
      explorationLoop state Idle

    _ -> do
      log "I dont understand that command."
      explorationLoop state Idle


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