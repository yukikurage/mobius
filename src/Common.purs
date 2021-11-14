module Common where

import Prelude

import Data.Array (range)
import Data.Maybe (Maybe)
import Matrix (Matrix)
import Matrix as Matrix

-- なんでこの関数がないんだよという関数のためのモジュール

-- | rangeの逆順リストを除いたver
upRange :: Int -> Int -> Array Int
upRange x y
  | x > y = []
  | otherwise = range x y

-- Matrixが何故か横からアクセスする仕様なので変更

get :: forall a. Int -> Int -> Matrix a -> Maybe a
get x y = Matrix.get y x

set :: forall a. Int -> Int -> a -> Matrix a -> Maybe (Matrix a)
set x y = Matrix.set y x

modify :: forall a. Int -> Int -> (a -> a) -> Matrix a -> Maybe (Matrix a)
modify x y = Matrix.modify y x

repeat :: forall a. Int -> Int -> a -> Matrix a
repeat x y = Matrix.repeat y x