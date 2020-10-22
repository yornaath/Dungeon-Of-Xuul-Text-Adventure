module Components.Game where

import Prelude

import Components.Test as Test
import Components.Utils (css)
import Data.Argonaut.Core as AC
import Data.Array (index, reverse)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Engine.Environment (Environment)
import Game.GameState (GameState, gameStateToJson)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Queue as Q
import Web.UIEvent.KeyboardEvent (code, ctrlKey)

type State = {
  currentInput :: String,
  game :: GameState,
  log :: Array String,
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
  | Log String a

data GameOutput = Noop_

gameComponent :: forall input cm. MonadEffect cm => Environment -> GameState -> H.Component HH.HTML GameQuery input GameOutput cm
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
        -- --HH.button [ HE.onClick \e -> Just ( Roll (d20 : Nil ))] [ HH.text "roll" ],
        -- HH.slot _dieselector 1 dieSelector {} (Just <<< OnSelectedDie),
        -- HH.button [ HE.onClick (\e -> Just (Roll)) ] [ HH.text "roll" ],
        -- HH.div_ [ HH.text $ show state ]
        HH.div [css "console"] [

            HH.div [css "log"] $ (\logLine -> HH.div [css "logline whitespace-pre"] [HH.text logLine] ) <$> reverse state.log,
            
            HH.input [
              css "prompt",
              HP.value state.currentInput,
              HE.onValueInput \str -> Just (Typing str),
              HE.onKeyUp \e -> do 
                let 
                  ctrl = ctrlKey e 
                  keyCode = code e
                case keyCode of 
                  "Enter" -> Just Return
                  "ArrowUp" -> Just HistoryUp
                  "ArrowDown" -> Just HistoryDown
                  "KeyC" -> if ctrl then Just Clear else Just Noop
                  "Escape" -> Just Clear
                  _ -> Just Noop
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
      
