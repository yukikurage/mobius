module Common where

import Prelude

import Data.Array (range)

-- なんでこの関数がないんだよという関数のためのモジュール

-- | rangeの逆順リストを除いたver
upRange :: Int -> Int -> Array Int
upRange x y
  | x > y = []
  | otherwise = range x y