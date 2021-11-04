module Components.Game where

import Prelude

import Components.Utils (css)
import Data.Argonaut.Core as AC
import Data.Array (index, reverse)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Engine.Environment (Environment)
import Game.GameState (GameState, gameStateToJson)
import Halogen as H
import Halogen.HTML (fromPlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Queue as Q
import Web.UIEvent.KeyboardEvent (code, ctrlKey)

type State = {
  currentInput :: String,
  game :: GameState,
  log :: Array (HH.HTML Void Void),
  history :: Array String,
  historyIndex:: Int
}

data Action a = 
    Init
  | Noop
  | Return
  | Typing String
  | Clear
  | HistoryUp
  | HistoryDown

data GameQuery a = 
    GameTurn GameState a
  | Log (HH.HTML Void Void) a

data GameOutput = Noop_

gameComponent :: forall input cm. MonadEffect cm => Environment -> GameState -> H.Component GameQuery input GameOutput cm
gameComponent environment initialGameState =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { 
        handleAction = handleAction, 
        handleQuery = handleQuery,
        initialize = Just Init
        }
    }
  where

  initialState :: _ State
  initialState _ = { currentInput: "", 
                     game: initialGameState,
                     log: [],
                     history: [],
                     historyIndex: 0
                   }

  render state =
    HH.div [css "container mx-auto px-4 pt-4"]
      [
       
        HH.div [css "console"] [

            HH.div [css "log"] $ fromPlainHTML <$> (reverse state.log),
            
            HH.input [
              css "prompt",
              HP.autofocus true,
              HP.value state.currentInput,
              HE.onValueInput \str -> (Typing str),
              HE.onKeyUp \e -> do 
                let 
                  ctrl = ctrlKey e 
                  keyCode = code e
                case keyCode of 
                  "Enter" -> Return
                  "ArrowUp" -> HistoryUp
                  "ArrowDown" -> HistoryDown
                  "KeyC" -> if ctrl then Clear else Noop
                  "Escape" -> Clear
                  _ -> Noop
            ]

        ],

        HH.div [css "debug"] [HH.text $ AC.stringifyWithIndent 2 $ gameStateToJson state.game]
      ]

  handleQuery 
    :: forall a q
     . GameQuery q
    -> H.HalogenM State (Action a) () GameOutput cm (Maybe q)
  handleQuery = case _ of 
    GameTurn state next -> do
      H.modify_ _ { game = state }
      pure (Just next)
    Log logLine next  -> do
      state <- H.get
      H.modify_ _ { log = state.log <> [logLine] }
      pure (Just next)

  handleAction = case _ of
    Init -> do
      { game } <- H.get
      pure unit
    Noop -> do
      pure unit
    Typing str -> do
      H.modify_ _ { currentInput = str }
      pure unit
    Return -> do
      { currentInput, history } <- H.get
      liftEffect $ Q.drain environment.input
      liftEffect $ Q.put environment.input currentInput
      H.modify_ _ { currentInput = "", historyIndex = 0, history = [currentInput] <> history  }
    Clear -> do
      H.modify_ _ { currentInput = "", historyIndex = 0}
    HistoryUp -> do
      { history, historyIndex } <- H.get
      let nextIndex = historyIndex + 1
      let historyItem = index history (nextIndex - 1)
      case historyItem of 
        Just str -> do
          H.modify_ _ { historyIndex = nextIndex, currentInput = str }
        _ -> do 
          pure unit
    HistoryDown -> do
      { history, historyIndex } <- H.get
      let nextIndex = if historyIndex > 0 then historyIndex - 1 else 0
      let historyItem = index history nextIndex
      case historyItem of 
        Just str -> do
          H.modify_ _ { historyIndex = nextIndex, currentInput = str }
        _ -> do 
          pure unit
      pure unit
      
