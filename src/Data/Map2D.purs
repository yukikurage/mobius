module Data.Map2D where

import Prelude

class
  Functor m <= Map2D m p where
  updateAt :: forall a. p -> a -> m a -> m a
  index :: forall a. m a -> p -> a
