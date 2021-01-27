module Chrome.Wrap where

import MasonPrelude
import Control.Monad.Except (ExceptT(..))
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError(..), decodeJson)
import Data.Bifunctor (bimap, lmap)
import Data.Maybe (fromJust)
import Data.MultiTuple (T3(..))
import Debug as Debug
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Partial.Unsafe (unsafePartial)

type Chrome a
  = ExceptT Error Aff a

data Error
  = Permission String
  | Decode JsonDecodeError

permission :: String -> Error
permission api = Permission $ "There is no `" <> api <> "` property on `chrome`. Check your permissions."

foreign import wrapApiImpl ::
  ∀ a.
  (∀ a' b' c'. c' -> a' \/ (b' \/ c')) ->
  (∀ a'. Error \/ a') ->
  String ->
  String ->
  Array Json ->
  Error \/ (a \/ Json) ->
  EffectFnAff (Error \/ (a \/ Json))

derive instance genericError :: Generic Error _

instance showError :: Show Error where
  show = genericShow

wrapApi :: ∀ a. DecodeJson a => String -> String -> Array Json -> Chrome a
wrapApi api method args =
  wrapFailableApi api method args Nothing
    <#> unsafePartial fromJust

wrapFailableApi :: ∀ a f. Applicative f => DecodeJson a => String -> String -> Array Json -> f a -> Chrome (f a)
wrapFailableApi api method args error =
  wrapApiImpl
    (Right <. Right)
    (Left $ permission api)
    api
    method
    args
    (Right $ Left error)
    # fromEffectFnAff
    # map case _ of
        Right result -> case result of
          Right json -> bimap Decode pure $ decodeJson json
          Left _ -> Right error
        Left e -> Left e
    # ExceptT

foreign import wrapListenerImpl ::
  (∀ a' b'. b' -> a' \/ b') ->
  (∀ a'. Error \/ a') ->
  String ->
  String ->
  EffectFnAff (Error \/ Json)

wrapListener :: ∀ a. DecodeJson a => String -> String -> Chrome a
wrapListener api event =
  wrapListenerImpl Right (Left $ permission api) api event
    # fromEffectFnAff
    # map case _ of
        Right json ->
          lmap Decode
            $ decodeJson json
            >>= case _ of
                [ a ] -> Right a
                _ -> Left $ TypeMismatch "wrapListener"
        Left e -> Left e
    # ExceptT

wrapListener3 :: ∀ a b c d. DecodeJson a => DecodeJson b => DecodeJson c => String -> String -> (a -> b -> c -> d) -> Chrome d
wrapListener3 api event f =
  wrapListenerImpl Right (Left $ permission api) api event
    # fromEffectFnAff
    # map case _ of
        Right json ->
          lmap Decode do
            T3 a b c <- decodeJson json
            pure $ f a b c
        Left e -> Left e
    # ExceptT
