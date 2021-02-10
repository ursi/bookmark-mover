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

type Api
  = String

data Error
  = Permission String
  | Decode MultipleErrors

permission :: String -> Error
permission api = Permission $ "There is no `" <> api <> "` property on `chrome`. Check your permissions."

foreign import wrapApiImpl ::
  ∀ a.
  (∀ a' b' c'. c' -> a' \/ (b' \/ c')) ->
  (∀ a'. Error \/ a') ->
  Api ->
  String ->
  Array Foreign ->
  Error \/ (a \/ Foreign) ->
  EffectFnAff (Error \/ (a \/ Foreign))

derive instance genericError :: Generic Error _

instance showError :: Show Error where
  show = genericShow

wrapApi :: ∀ a. ReadForeign a => Api -> String -> Array Foreign -> Chrome a
wrapApi api method args =
  wrapFailableApi api method args Nothing
    <#> unsafePartial fromJust

wrapFailableApi :: ∀ a f. Applicative f => ReadForeign a => Api -> String -> Array Foreign -> f a -> Chrome (f a)
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
  Api ->
  String ->
  EffectFnAff (Error \/ Foreign)

wrapListenerNHelper :: ∀ a. (Foreign -> Json.E a) -> Api -> String -> Chrome a
wrapListenerNHelper f api =
  wrapListenerImpl Right (Left $ permission api) api
    .> fromEffectFnAff
    .> map case _ of
        Right json -> lmap Decode $ f json
        Left e -> Left e
    .> ExceptT

wrapListener :: ∀ a. ReadForeign a => Api -> String -> Chrome a
wrapListener = wrapHelper wrapListenerNHelper

wrapListener2 :: ∀ a b c. ReadForeign a => ReadForeign b => (a -> b -> c) -> Api -> String -> Chrome c
wrapListener2 = wrap2Helper wrapListenerNHelper

wrapListener3 :: ∀ a b c d. ReadForeign a => ReadForeign b => ReadForeign c => (a -> b -> c -> d) -> Api -> String -> Chrome d
wrapListener3 = wrap3Helper wrapListenerNHelper

wrapHelper ::
  ∀ a b.
  ReadForeign a =>
  ((Foreign -> Json.E a) -> String -> String -> b) ->
  String ->
  String ->
  b
wrapHelper w =
  w \json ->
    Json.read json
      >>= case _ of
          [ a ] -> Right a
          _ -> Left $ pure $ TypeMismatch "wrapListener" "not an array with a single element"

wrap2Helper ::
  ∀ a b c m.
  ReadForeign a =>
  ReadForeign b =>
  (∀ a'. (Foreign -> Json.E a') -> String -> String -> m a') ->
  (a -> b -> c) ->
  String ->
  String ->
  m c
wrap2Helper w f =
  w \json -> do
    T2 a b <- Json.read json
    pure $ f a b

wrap3Helper ::
  ∀ a b c d m.
  ReadForeign a =>
  ReadForeign b =>
  ReadForeign c =>
  (∀ a'. (Foreign -> Json.E a') -> String -> String -> m a') ->
  (a -> b -> c -> d) ->
  String ->
  String ->
  m d
wrap3Helper w f =
  w \json -> do
    T3 a b c <- Json.read json
    pure $ f a b c
