module Mobius.MapState where

import Prelude

import Data.Maybe (Maybe(..))
import Mobius.Directions (Directions)
import Mobius.LatticePoint (LatticePoint)
import Mobius.Map2D (Cell(..), Map2D, WithSingularPoint(..), compute, index, updateAt)
import Mobius.Object (Object, ObjectProperty(..), objectProperty)

data MapState = MapState
  { map2D :: Map2D (Cell Object)
  , character :: LatticePoint
  , destinations :: Map2D Boolean
  }

instance Show MapState where
  show (MapState { map2D, character, destinations }) = "MapState{\nmap2D:\n"
    <> show map2D
    <> ",\ncharacter:"
    <> show character
    <> ",\ndestinations:\n"
    <> show destinations

move :: MapState -> Directions -> MapState
move (MapState { map2D, character, destinations }) d = MapState $ case compute map2D character d of
  Nothing -> { map2D, character, destinations } --今いる場所が特異点ならすすめない
  Just nextPos -> case index map2D nextPos of --目の前が
    Just SingularPoint -> { map2D, character, destinations } --特異点ならすすめない
    Just (NotSingularPoint Empty) ->
      { map2D, character: nextPos, destinations } --何も無いならすすめる
    Just (NotSingularPoint (Object x)) -> case objectProperty x of
      Stop -> { map2D, character, destinations } --Stopがあるならすすめない
      Push -> case compute map2D nextPos d of --Pushがあるなら
        Nothing -> { map2D, character, destinations } --その場所が特異点ならすすめない(そんなことはない)
        Just doubleNextPos -> case index map2D doubleNextPos of --そのさらに前が
          Just (NotSingularPoint Empty) -> { map2D: newMap2D, character: nextPos, destinations } --何も無いならすすめる
            where
            newMap2D = updateAt doubleNextPos (Object x)
              $ updateAt nextPos Empty map2D -- 次のMap2Dの状態
          _ -> { map2D, character, destinations } --それ以外ならすすめない
    Nothing -> { map2D, character, destinations } --範囲外ならすすめない