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
import Lib.AffReadline (question)
import Node.ReadLine as RL
import Static.Text as StaticText

main :: Effect Unit
main = do

  interface <- liftEffect $ RL.createConsoleInterface RL.noCompletion

  log StaticText.intro

  let 

    initialState :: GameState
    initialState = (CreatingCharacter { name: Nothing, role: Nothing })

    env :: GameEnvironment
    env = { interface }

    gameLoopRunner :: GameState -> Aff Unit
    gameLoopRunner currentState = do

      line <- interface # question "> "

      let 
        command :: Array String
        command = (split (wrap " ")) line

      newState <- runGame env (game currentState command)

      gameLoopRunner newState
          

  launchAff_ $ (gameLoopRunner initialState)