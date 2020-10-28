module Main where

import Prelude

import Components.Game (GameQuery(..), gameComponent)
import Effect (Effect)
import Effect.Aff (Aff, forkAff, launchAff)
import Engine.Environment (Environment)
import Engine.Input as EngineInput
import Engine.Log as EngineLog
import Engine.SaveGames (loadGame)
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

  input <- liftEffect $ EngineInput.empty
  log <- liftEffect $ EngineLog.empty

  let
  
    environment :: Environment
    environment = { input, log }

    initialGameState :: GameState
    initialGameState = MainMenu

  c <- loadGame "blytz"

  body <- HA.awaitBody
  
  halogenIO <- runUI (gameComponent environment c) unit body

  void $ forkAff $ do
    let 
      gameLoopRunner :: GameState -> Aff Unit
      gameLoopRunner currentState = do
        newState <- runEngine environment (gameLoop currentState)
        _ <- halogenIO.query $ H.tell $ GameTurn newState
        gameLoopRunner newState
    gameLoopRunner c
  
  void $ forkAff $ do
    liftEffect $ Q.on log \line -> do
      _ <- launchAff $ do 
        _ <- halogenIO.query $ H.tell $ Log line
        pure unit
      pure unit
    