module Data.MultiTuple where

import MasonPrelude
import Control.Monad.Except (throwError)
import Data.Array as Array
import Debug as Debug
import Foreign (ForeignError(..))
import Simple.JSON (class ReadForeign, readImpl)

data T2 a b
  = T2 a b

instance readForeignT2 :: (ReadForeign a, ReadForeign b) => ReadForeign (T2 a b) where
  readImpl =
    readImpl
      >=> case _ of
          [ a, b ] ->
            lift2 T2
              (readImpl a)
              (readImpl b)
          a ->
            throwError $ pure
              $ TypeMismatch
                  "array of length 2"
                  ("array of length " <> show (Array.length a))

data T3 a b c
  = T3 a b c

instance readForeignT3 :: (ReadForeign a, ReadForeign b, ReadForeign c) => ReadForeign (T3 a b c) where
  readImpl =
    readImpl
      >=> case _ of
          [ a, b, c ] ->
            lift3 T3
              (readImpl a)
              (readImpl b)
              (readImpl c)
          a ->
            throwError $ pure
              $ TypeMismatch
                  "array of length 3"
                  ("array of length " <> show (Array.length a))
