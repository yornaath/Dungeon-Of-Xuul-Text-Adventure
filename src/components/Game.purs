module Components.Game where

import Prelude

import Components.Utils (css)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Engine.Environment (Environment)
import Game.GameState (GameState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Queue as Q
import Web.UIEvent.KeyboardEvent (code)
import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (error)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Query.EventSource (EventSource)
import Halogen.Query.EventSource as EventSource
import Halogen.VDom.Driver (runUI)

type State = {
  currentInput :: String,
  game :: GameState
}

data Action a = 
    Init
  | Noop
  | Turn GameState a
  | Log String a
  | Return
  | Typing String

gameComponent :: forall query input output cm. MonadEffect cm => Environment -> GameState -> H.Component HH.HTML query input output cm
gameComponent environment initialGameState =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Init }
    }
  where

  initialState :: _ State
  initialState _ = { currentInput: "", game: initialGameState }

  render state =
    HH.div [css "container mx-auto px-4 pt-4"]
      [
        -- --HH.button [ HE.onClick \e -> Just ( Roll (d20 : Nil ))] [ HH.text "roll" ],
        -- HH.slot _dieselector 1 dieSelector {} (Just <<< OnSelectedDie),
        -- HH.button [ HE.onClick (\e -> Just (Roll)) ] [ HH.text "roll" ],
        -- HH.div_ [ HH.text $ show state ]
        HH.input [
          css "bg-blue-500",
          HP.value state.currentInput,
          HE.onValueInput \str -> Just (Typing str),
          HE.onKeyUp \e -> do
            if code e == "Enter" then 
              Just Return
            else 
              Just Noop
        ]
      ]

  handleAction = case _ of
    Init -> do
      { game } <- H.get
      pure unit
    Noop -> do
      pure unit
    Turn nextState a -> do
      H.modify_ _ { game = nextState }
      pure unit
    Log str a -> do
      log  ("LOG: " <> str )
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
