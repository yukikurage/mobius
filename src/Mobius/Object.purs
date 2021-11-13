module Mobius.Object where

import Prelude

data Object
  = Wall
  | Apple
  | Box

data ObjectProperty = Push | Stop

derive instance Eq Object
derive instance Eq ObjectProperty

instance Show Object where
  show = case _ of
    Wall -> "⬛"
    Apple -> "🍏"
    Box -> "⬜"

objectProperty :: Object -> ObjectProperty
objectProperty = case _ of
  Wall -> Stop
  Apple -> Push
  Box -> Push