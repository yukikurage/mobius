module Data.LatticePoint where

class LatticePoint p where
  up :: p -> p
  right :: p -> p
  down :: p -> p
  left :: p -> p
