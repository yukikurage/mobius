module Mobius.Structure where

data Bridge = Bridge Int Int

data Structure = Structure
  { isBridge :: Int -> Int -> Boolean
  , isSingularPoint :: Int -> Int -> Boolean
  }