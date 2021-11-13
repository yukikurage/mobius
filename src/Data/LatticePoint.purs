module Data.LatticePoint where

import Data.Directions (Directions)

class LatticePoint p where
  move :: Directions -> p -> p
