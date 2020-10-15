module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String (split)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Game.Game (runGame)
import Game.GameEnvironment (GameEnvironment)
import Game.GameState (GameState(..))
import Game.Loop.Root (game)
import Lib.AffReadline (command, question)
import Node.ReadLine as RL
import Static.Text as StaticText

main :: Effect Unit
main = do

  interface <- liftEffect $ RL.createConsoleInterface RL.noCompletion

  log StaticText.intro

  let 

    initialState :: GameState
    initialState = (MainMenu)

    env :: GameEnvironment
    env = { interface }

    gameLoopRunner :: GameState -> Aff Unit
    gameLoopRunner currentState = do
      command <- command interface "> "
      newState <- runGame env (game currentState command)
      gameLoopRunner newState
          

  launchAff_ $ (gameLoopRunner initialState)