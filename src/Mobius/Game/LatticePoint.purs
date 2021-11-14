module Mobius.Game.LatticePoint where

import Prelude

import Mobius.Game.Surface (Surface)

data LatticePoint = LatticePoint Surface Int Int -- | layer, posX, posY

instance Show LatticePoint where
  show (LatticePoint s i j) = "LatticePoint " <> show s <> " " <> show i <> " " <> show j