module Components.DieSelector where

import Prelude

import Components.Utils (css)
import Data.Array (fromFoldable)
import Game.Data.Die (Die, allDies)
import Data.List (List(..), delete, snoc)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type DieSelectorSlot = H.Slot DieSelectorQuery DieSelectorOutput

_dieselector = SProxy :: SProxy "dieselector"

type DieSelectorState = {
  selected :: List Die
}

data DieSelectorAction = 
    Select Die
  | Deselect Die

type DieSelectorOutput = List Die

data DieSelectorQuery a = 
  GetSelected ((List Die) -> a)

dieSelector :: forall input cm. MonadEffect cm => H.Component HH.HTML DieSelectorQuery input DieSelectorOutput cm
dieSelector =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, handleQuery = handleQuery }
    }
  where
  initialState :: _ DieSelectorState
  initialState _ = { selected: Nil }

  render state =
    HH.div_
      [
        HH.div [ ] (map (\d ->
          HH.div [ css "flex" ] 
            [
              HH.button [ css "p-1" , HE.onClick (\e -> Just (Select d)) ] [ HH.text "+" ],
              HH.div [css "p-1"] [ HH.text $ show d ],
              HH.button [ css "p-1", HE.onClick (\e -> Just (Deselect d)) ] [ HH.text "-" ]
            ]
        ) (fromFoldable allDies))
        ,
        HH.div [ css "flex" ] (map (\d -> 
          HH.div [ css "mr-2 p-1" ] 
            [
              HH.text $ show d
            ]
        ) (fromFoldable state.selected))
      ]

  handleQuery
    :: forall a
     . DieSelectorQuery a
    -> H.HalogenM DieSelectorState DieSelectorAction () DieSelectorOutput cm (Maybe a)
  handleQuery = case _ of

    -- When we receive a the request-style `GetEnabled` query, which requires
    -- a boolean result, we get a boolean from our state and reply with it.
    GetSelected reply -> do
      { selected } <- H.get
      pure (Just (reply selected))

  handleAction :: DieSelectorAction -> H.HalogenM DieSelectorState DieSelectorAction () DieSelectorOutput cm Unit
  handleAction = case _ of
    Select die -> do
      H.modify_ \state -> { selected: snoc state.selected die }
      { selected } <- H.get
      H.raise (selected)
    Deselect die -> do
      H.modify_ \state -> { selected: delete die state.selected }
      { selected } <- H.get
      H.raise (selected)
