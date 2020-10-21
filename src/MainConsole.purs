module MainConsole where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Game.Engine (runEngine)
import Engine.Environment (Environment)
import Game.GameState (GameState(..))
import Game.Loop.Root (gameLoop)
import Engine.SaveGames (loadGame)
import Queue as Q

initialState :: GameState
initialState = (MainMenu)

main :: Effect Unit
main = launchAff_ do

  input <- liftEffect $ Q.new
  log <- liftEffect $ Q.new
  save <- loadGame "dia"

  let 

    env :: Environment
    env = { input, log }

    gameLoopRunner :: GameState -> Aff Unit
    gameLoopRunner currentState = do
      newState <- runEngine env (gameLoop currentState)
      gameLoopRunner newState

  (gameLoopRunner initialState)