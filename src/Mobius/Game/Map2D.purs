module Mobius.Game.Map2D where

import Prelude

import Common (get, modify, set, upRange)
import Data.Array (filter, length)
import Data.Int (odd)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Matrix (Matrix, height, width)
import Mobius.Game.Directions (Directions(..))
import Mobius.Game.LatticePoint (LatticePoint(..))
import Mobius.Game.Surface (Surface(..), changeSurface)

data Cell a = Empty | Object a

derive instance Eq a => Eq (Cell a)
derive instance Functor Cell

instance Show a => Show (Cell a) where
  show = case _ of
    Empty -> "＿"
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
  show (Map2D m) = deleteComma (show front) <> "\n\n" <> deleteComma (show back) <> "\n"
    where
    front = map (map fst) m
    back = map (map snd) m
    deleteComma = String.replaceAll (String.Pattern ",") (String.Replacement "")

-- | Nothingのときは範囲外
index :: forall a. Map2D a -> LatticePoint -> Maybe (WithSingularPoint a)
index (Map2D m) (LatticePoint s i j) = map (map f) $ get i j m
  where
  f (Tuple front back) = if s == Front then front else back

-- | Nothingのときは特異点(mobius)から上下左右に移動しようとしている
-- | それ以外の場合は計算可能である
compute :: forall a. Eq a => Map2D a -> LatticePoint -> Directions -> Maybe LatticePoint
compute m p _ | index m p == Just SingularPoint = Nothing
compute _ (LatticePoint s i j) Change = Just $ LatticePoint (changeSurface s) i j
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

inRange :: forall a. Map2D a -> LatticePoint -> Boolean
inRange (Map2D m) (LatticePoint _ i j) = 0 <= i && i < height m && 0 <= j && j < width m

-- | 指定した場所を更新．
-- | 特異点は更新できない
updateAt :: forall a. LatticePoint -> a -> Map2D a -> Map2D a
updateAt (LatticePoint s i j) x (Map2D m) = Map2D $ fromMaybe m $ modify i j f m
  where
  f SingularPoint = SingularPoint
  f (NotSingularPoint (y /\ z)) = NotSingularPoint $ if s == Front then x /\ z else y /\ x

-- | 指定した場所の特異点を削除して更新．
-- | 指定した場所に特異点が無かった場合，そのまま返す
updateSingularPoint :: forall a. Int -> Int -> Tuple a a -> Map2D a -> Map2D a
updateSingularPoint i j x (Map2D m) = Map2D $ fromMaybe m $ modify i j f m
  where
  f SingularPoint = NotSingularPoint x
  f t = t

makeSingularPoint :: forall a. Int -> Int -> Map2D a -> Map2D a
makeSingularPoint i j (Map2D m) = Map2D $ fromMaybe m $ set i j SingularPoint m