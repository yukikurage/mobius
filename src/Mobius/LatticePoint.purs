module Mobius.LatticePoint where

import Mobius.Directions (Directions)

class LatticePoint p where
  move :: Directions -> p -> p
