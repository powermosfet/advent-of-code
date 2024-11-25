module Data.MyStack where

import Prelude

import Data.Foldable (class Foldable)
import Data.List (List, (:))
import Data.List as List

data Stack a = Stack (List a)
instance eqStack :: Eq a => Eq (Stack a) where
  eq (Stack as) (Stack bs) = as == bs
instance showStack :: Show a => Show (Stack a) where
  show (Stack as)  = "Stack.fromFoldable " <> show as

push :: forall a. a -> Stack a -> Stack a
push item (Stack stackItems) =
  Stack (item : stackItems)

empty :: forall a. Stack a
empty =
  Stack List.Nil

fromFoldable :: forall f a. Foldable f => f a -> Stack a
fromFoldable items =
  Stack (List.fromFoldable items)
