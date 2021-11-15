module Mobius.Game.LatticePoint where

import Prelude

import Mobius.Game.Directions (Directions(..))
import Mobius.Game.Surface (Surface)

data LatticePoint = LatticePoint Surface Int Int -- | layer, posX, posY

data Point = Point Int Int

instance Show LatticePoint where
  show (LatticePoint s i j) = "LatticePoint " <> show s <> " " <> show i <> " " <> show j

toPoint :: LatticePoint -> Point
toPoint (LatticePoint _ i j) = Point i j

fromPoint :: Surface -> Point -> LatticePoint
fromPoint s (Point i j) = LatticePoint s i j

computeWithPoint :: Point -> Directions -> Point
computeWithPoint (Point i j) = case _ of
  Up -> Point (i - 1) j
  Down -> Point (i + 1) j
  Right -> Point i $ j + 1
  Left -> Point i $ j - 1
  Change -> Point i j
