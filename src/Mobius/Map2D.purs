module Mobius.Map2D where

import Prelude

import Common (upRange)
import Data.Array (filter, length)
import Data.Int (odd)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Matrix (Matrix, get, height, width)
import Mobius.Directions (Directions(..))
import Mobius.LatticePoint (LatticePoint(..))
import Mobius.Surface (Surface(..))

data Cell a = Empty | Object a

derive instance Eq a => Eq (Cell a)
derive instance Functor Cell

instance Show a => Show (Cell a) where
  show = case _ of
    Empty -> "　"
    Object a -> show a

data WithSingularPoint a = SingularPoint | NotSingularPoint a

derive instance Eq a => Eq (WithSingularPoint a)
derive instance Functor WithSingularPoint

instance Show a => Show (WithSingularPoint a) where
  show = case _ of
    SingularPoint -> "🌀"
    NotSingularPoint a -> show a

data Map2D a = Map2D (Matrix (WithSingularPoint (Tuple a a)))

instance Show a => Show (Map2D a) where
  show (Map2D m) = show front <> "\n\n" <> show back <> "\n"
    where
    front = map (map fst) m
    back = map (map snd) m

-- | Nothingのときはなんにも無い
index :: forall a. Map2D a -> LatticePoint -> Maybe (WithSingularPoint a)
index (Map2D m) (LatticePoint s i j) = map (map f) $ get i j m
  where
  f (Tuple front back) = if s == Front then front else back

-- | Nothingのときは特異点(mobius)から上下左右に移動しようとしている
-- | それ以外の場合は計算可能である
compute :: forall a. Eq a => Map2D a -> LatticePoint -> Directions -> Maybe LatticePoint
compute m p _ | index m p == Just SingularPoint = Nothing
compute m (LatticePoint s i j) d = Just $ LatticePoint newS newI newJ
  where
  isCrossBridge = case d of
    Left -> length $ filter (\k -> index m (LatticePoint s k j) == Just SingularPoint) (upRange 0 i)
    Right -> length $ filter (\k -> index m (LatticePoint s k (j + 1)) == Just SingularPoint) (upRange 0 i)
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

inRange :: forall a. Map2D a -> LatticePoint -> Boolean
inRange (Map2D m) (LatticePoint _ i j) = 0 <= i && i < height m && 0 <= j && j < width m