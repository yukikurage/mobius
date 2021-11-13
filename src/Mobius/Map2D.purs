module Mobius.Map2D where

import Prelude

import Common (upRange)
import Data.Array (filter, length)
import Data.Int (odd)
import Data.Maybe (Maybe(..))
import Mobius.Directions (Directions(..))
import Mobius.LatticePoint (LatticePoint(..))
import Mobius.Surface (Surface(..))

data Map2D a = Map2D
  { height :: Int
  , width :: Int
  , objects :: LatticePoint -> Maybe a
  , destinations :: LatticePoint -> Boolean --目的地
  , mobius :: Int -> Int -> Boolean
  , character :: LatticePoint
  }

data WithMobius a = Mobius | NotMobius a

derive instance Eq a => Eq (WithMobius a)

index :: forall a. Map2D a -> LatticePoint -> Maybe (WithMobius a)
index (Map2D { mobius }) (LatticePoint _ i j) | mobius i j = Just Mobius
index (Map2D { objects }) p = map NotMobius $ objects p

-- | Nothingのときは特異点(mobius)から上下左右に移動しようとしている
-- | それ以外の場合は計算可能である
compute :: forall a. Eq a => Map2D a -> Directions -> LatticePoint -> Maybe LatticePoint
compute m _ p | index m p == Just Mobius = Nothing
compute m d (LatticePoint s i j) = Just $ LatticePoint newS newI newJ
  where
  isCrossBridge = case d of
    Left -> length $ filter (\k -> index m (LatticePoint s k j) == Just Mobius) (upRange 0 i)
    Right ->length $ filter (\k -> index m (LatticePoint s k (j + 1)) == Just Mobius) (upRange 0 i)
    _ -> 0
  newS = if odd isCrossBridge then changeSurface s else s
  newI = case d of
    Up -> i - 1
    Down -> i + 1
    _ -> i
  newJ = case d of
    Right -> j + 1
    Left -> j - 1
    _ -> j
  changeSurface Front = Back
  changeSurface Back = Front

-- move :: Directions -> Map2D Object -> Map2D Object
-- move d