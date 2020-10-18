module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Game.Engine (runEngine)
import Game.GameEnvironment (GameEnvironment)
import Game.GameState (GameState(..))
import Game.Loop.Root (game)
import Node.ReadLine as RL
import Static.Text as StaticText

main :: Effect Unit
main = do

  interface <- liftEffect $ RL.createConsoleInterface RL.noCompletion

  log StaticText.banner

  let 

    initialState :: GameState
    initialState = (MainMenu)

    env :: GameEnvironment
    env = { interface }

    gameLoopRunner :: GameState -> Aff Unit
    gameLoopRunner currentState = do
      newState <- runEngine env (game currentState)
      gameLoopRunner newState
          

  launchAff_ $ (gameLoopRunner initialState)