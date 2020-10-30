module Game.Data.Item.Hands where

import Prelude
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Game.Data.Item.Equipment
  ( HandItem(..)
  , MainHandItem
  , OffHandItem
  , EitherHandItem
  , TwoHandedItem
  )

data Wielding
  = None
  | MainHand (MainHandItem \/ EitherHandItem)
  | OffHand (OffHandItem \/ EitherHandItem)
  | BothHands (MainHandItem \/ EitherHandItem) (OffHandItem \/ EitherHandItem)
  | TwoHanded TwoHandedItem

data EitherMode
  = Main
  | Off
  | Max

equip :: Boolean -> EitherMode -> HandItem -> Wielding -> Wielding
equip moveEither eitherMode item wielding = case item, wielding of
  MainHandItem i, None -> MainHand $ Left i
  OffHandItem i, None -> OffHand $ Left i
  EitherHandItem i, None -> case eitherMode of
    Main -> MainHand $ Right i
    Off -> OffHand $ Right i
    Max -> MainHand $ Right i
  MainHandItem i, MainHand m -> case m of
    Left _ -> MainHand $ Left i
    Right e ->
      if moveEither then
        BothHands (Left i) (Right e)
      else
        MainHand $ Left i
  OffHandItem i, MainHand m -> BothHands m $ Left i
  EitherHandItem i, MainHand m -> case eitherMode of
    Main -> case m of
      Left _ -> MainHand $ Right i
      Right e ->
        if moveEither then
          BothHands (Right i) (Right e)
        else
          MainHand $ Right i
    Off -> BothHands m $ Right i
    Max -> BothHands m $ Right i
  MainHandItem m, OffHand o -> BothHands (Left m) o
  OffHandItem i, OffHand o -> case o of
    Left _ -> OffHand $ Left i
    Right e ->
      if moveEither then
        BothHands (Right e) (Left i)
      else
        OffHand $ Left i
  EitherHandItem i, OffHand o -> case eitherMode of
    Main -> BothHands (Right i) o
    Off -> case o of
      Left _ -> OffHand $ Right i
      Right e ->
        if moveEither then
          BothHands (Right e) (Right i)
        else
          OffHand $ Right i
    Max -> BothHands (Right i) o
  MainHandItem i, BothHands _ o -> BothHands (Left i) o
  OffHandItem i, BothHands m _ -> BothHands m $ (Left i)
  EitherHandItem i, BothHands m o -> case eitherMode of
    Main -> BothHands (Right i) o
    Off -> BothHands m (Right i)
    Max -> BothHands (Right i) o
  MainHandItem i, TwoHanded _ -> MainHand $ Left i
  OffHandItem i, TwoHanded _ -> OffHand $ Left i
  EitherHandItem i, TwoHanded _ -> case eitherMode of
    Main -> MainHand $ Right i
    Off -> OffHand $ Right i
    Max -> MainHand $ Right i
  TwoHandedItem i, _ -> TwoHanded i
