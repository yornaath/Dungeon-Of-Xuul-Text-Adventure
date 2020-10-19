module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Game.Engine (runEngine)
import Game.GameEnvironment (GameEnvironment)
import Game.GameState (GameState(..))
import Game.Loop.Root (game)
import Game.Saving (loadGame)
import Node.ReadLine as RL

initialState :: GameState
initialState = (MainMenu)

main :: Effect Unit
main = launchAff_ do

  interface <- liftEffect $ RL.createConsoleInterface RL.noCompletion
  save <- loadGame "dia"

  let 

    env :: GameEnvironment
    env = { interface }

    gameLoopRunner :: GameState -> Aff Unit
    gameLoopRunner currentState = do
      newState <- runEngine env (game currentState)
      gameLoopRunner newState
          

  (gameLoopRunner initialState)