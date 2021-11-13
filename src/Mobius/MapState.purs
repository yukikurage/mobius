module Mobius.MapState where

import Prelude

import Data.Maybe (Maybe(..))
import Mobius.Directions (Directions)
import Mobius.LatticePoint (LatticePoint)
import Mobius.Map2D (Cell(..), Map2D, WithSingularPoint(..), compute, index)
import Mobius.Object (Object)

data MapState = MapState
  { map2d :: Map2D (Cell Object)
  , character :: LatticePoint
  , destinations :: Map2D Boolean
  }

instance Show MapState where
  show (MapState { map2d, character, destinations }) = "MapState{\nmap2d:\n"
    <> show map2d
    <> ",\ncharacter:"
    <> show character
    <> ",\ndestinations:\n"
    <> show destinations

move :: MapState -> Directions -> MapState
move (MapState { map2d, character, destinations }) d = case compute map2d character d of
  Nothing -> MapState { map2d, character, destinations }
  Just nextPos -> case index map2d nextPos of
    Just (NotSingularPoint Empty) -> MapState { map2d, character: nextPos, destinations } --何も無いならすすめる
    _ -> MapState { map2d, character, destinations }