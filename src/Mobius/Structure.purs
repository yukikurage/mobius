module Mobius.Structure where

data Structure = Structure
  { isSwitch :: Int -> Int -> Boolean
  , isSingularPoint :: Int -> Int -> Boolean
  , isOutSide :: Int -> Int -> Boolean
  }