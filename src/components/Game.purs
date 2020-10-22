module Components.Game where

import Prelude

import Components.Utils (css)
import Data.Argonaut.Core as AC
import Data.Array (reverse)
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
import Web.UIEvent.KeyboardEvent (code)

type State = {
  currentInput :: String,
  game :: GameState,
  log :: Array String
}

data Action a = 
    Init
  | Noop
  | Return
  | Typing String

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
                     log: []
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
                if code e == "Enter" then 
                  Just Return
                else 
                  Just Noop
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
      { currentInput } <- H.get
      liftEffect $ Q.drain environment.input
      liftEffect $ Q.put environment.input currentInput
      H.modify_ _ { currentInput = "" }
      pure unit
