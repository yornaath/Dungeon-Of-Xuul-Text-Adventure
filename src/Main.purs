module Main where

import Prelude

import Components.Game (Action(..), gameComponent)
import Effect (Effect)
import Effect.Aff (Aff, forkAff)
import Engine.Environment (Environment)
import Game.Engine (runEngine)
import Game.GameState (GameState(..))
import Game.Loop.Root (gameLoop)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import MainConsole (initialState)
import Queue as Q

main :: Effect Unit
main = HA.runHalogenAff do

  input <- liftEffect $ Q.new
  log <- liftEffect $ Q.new

  let
  
    environment :: Environment
    environment = { input, log }

    initialGameState :: GameState
    initialGameState = MainMenu

  body <- HA.awaitBody
  
  halogenIO <- runUI (gameComponent environment initialGameState) unit body

  void $ forkAff $ do
    let 
      gameLoopRunner :: GameState -> Aff Unit
      gameLoopRunner currentState = do
        newState <- runEngine environment (gameLoop currentState)
        _ <- halogenIO.query $ H.tell $ Turn newState
        gameLoopRunner newState
    gameLoopRunner initialState
    
  pure unit