module Components.App where

import Prelude

import Components.DieSelector (DieSelectorOutput, DieSelectorQuery(..), DieSelectorSlot, _dieselector, dieSelector)
import Components.Utils (css)
import Data.Die (sumDies, tossDies)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = Int

data Action = 
    Roll
  | OnSelectedDie DieSelectorOutput

type Slots = ( dieselector :: DieSelectorSlot Int )

appComponent :: forall query input output cm. MonadEffect cm => H.Component HH.HTML query input output cm
appComponent =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: _ State
  initialState _ = 0

  render state =
    HH.div [css "container mx-auto px-4 pt-4"]
      [
        --HH.button [ HE.onClick \e -> Just ( Roll (d20 : Nil ))] [ HH.text "roll" ],
        HH.slot _dieselector 1 dieSelector {} (Just <<< OnSelectedDie),
        HH.button [ HE.onClick (\e -> Just (Roll)) ] [ HH.text "roll" ],
        HH.div_ [ HH.text $ show state ]
      ]

  handleAction = case _ of
    Roll -> do
      selectedDiesQuery <- H.query _dieselector 1 $ H.request GetSelected
      let dies = fromMaybe Nil selectedDiesQuery
      let tossed = tossDies dies
      sum <- H.liftEffect (sumDies tossed)
      H.modify_ \_ -> sum
    OnSelectedDie dies -> do
      H.modify_ \s -> s
