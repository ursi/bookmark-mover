module Chrome.Wrap where

import MasonPrelude
import Control.Monad.Except (ExceptT(..))
import Data.Bifunctor (bimap, lmap)
import Data.Maybe (fromJust)
import Data.MultiTuple (T2(..), T3(..))
import Debug as Debug
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Foreign (Foreign, ForeignError(..), MultipleErrors)
import Partial.Unsafe (unsafePartial)
import Simple.JSON (class ReadForeign)
import Simple.JSON as Json

type Chrome a
  = ExceptT Error Aff a

data Error
  = Permission String
  | Decode MultipleErrors

permission :: String -> Error
permission api = Permission $ "There is no `" <> api <> "` property on `chrome`. Check your permissions."

foreign import wrapApiImpl ::
  ∀ a.
  (∀ a' b' c'. c' -> a' \/ (b' \/ c')) ->
  (∀ a'. Error \/ a') ->
  String ->
  String ->
  Array Foreign ->
  Error \/ (a \/ Foreign) ->
  EffectFnAff (Error \/ (a \/ Foreign))

derive instance genericError :: Generic Error _

instance showError :: Show Error where
  show = genericShow

wrapApi :: ∀ a. ReadForeign a => String -> String -> Array Foreign -> Chrome a
wrapApi api method args =
  wrapFailableApi api method args Nothing
    <#> unsafePartial fromJust

wrapFailableApi :: ∀ a f. Applicative f => ReadForeign a => String -> String -> Array Foreign -> f a -> Chrome (f a)
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
          Right json -> bimap Decode pure $ Json.read json
          Left _ -> Right error
        Left e -> Left e
    # ExceptT

foreign import wrapListenerImpl ::
  (∀ a' b'. b' -> a' \/ b') ->
  (∀ a'. Error \/ a') ->
  String ->
  String ->
  EffectFnAff (Error \/ Foreign)

wrapListener :: ∀ a. ReadForeign a => String -> String -> Chrome a
wrapListener api event =
  wrapListenerImpl Right (Left $ permission api) api event
    # fromEffectFnAff
    # map case _ of
        Right json ->
          lmap Decode
            $ Json.read json
            >>= case _ of
                [ a ] -> Right a
                _ -> Left $ pure $ TypeMismatch "wrapListener" "not an array with a single element"
        Left e -> Left e
    # ExceptT

wrapListener2 :: ∀ a b c. ReadForeign a => ReadForeign b => String -> String -> (a -> b -> c) -> Chrome c
wrapListener2 api event f =
  wrapListenerImpl Right (Left $ permission api) api event
    # fromEffectFnAff
    # map case _ of
        Right json ->
          lmap Decode do
            T2 a b <- Json.read json
            pure $ f a b
        Left e -> Left e
    # ExceptT

wrapListener3 :: ∀ a b c d. ReadForeign a => ReadForeign b => ReadForeign c => String -> String -> (a -> b -> c -> d) -> Chrome d
wrapListener3 api event f =
  wrapListenerImpl Right (Left $ permission api) api event
    # fromEffectFnAff
    # map case _ of
        Right json ->
          lmap Decode do
            T3 a b c <- Json.read json
            pure $ f a b c
        Left e -> Left e
    # ExceptT
