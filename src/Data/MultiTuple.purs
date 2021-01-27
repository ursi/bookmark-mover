module Data.MultiTuple where

import MasonPrelude
import Control.Monad.Except (ExceptT(..))
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError(..), (.:), decodeJson)
import Data.Bifunctor (bimap)
import Data.Maybe (fromJust)
import Debug as Debug
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Partial.Unsafe (unsafePartial)

data T3 a b c
  = T3 a b c

instance decodeJsonT3 :: (DecodeJson a, DecodeJson b, DecodeJson c) => DecodeJson (T3 a b c) where
  decodeJson =
    decodeJson
      >=> case _ of
          [ a, b, c ] ->
            lift3 T3
              (decodeJson a)
              (decodeJson b)
              (decodeJson c)
          _ -> Left $ TypeMismatch "T3"
