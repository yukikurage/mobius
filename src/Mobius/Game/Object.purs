module Mobius.Game.Object where

import Prelude

import Mobius.Game.Drawable (class Drawable)
import Mobius.Game.Common (drawImagesFromImages)

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

instance Drawable Object where
  draw ctx images = case _ of
    Wall -> drawImagesFromImages ctx images "wall"
    Apple -> drawImagesFromImages ctx images "apple"
    Box -> drawImagesFromImages ctx images "box"

objectProperty :: Object -> ObjectProperty
objectProperty = case _ of
  Wall -> Stop
  Apple -> Push
  Box -> Push

objectImageSrc :: Object -> String
objectImageSrc = case _ of
  Wall -> "/images/cells/wall.png"
  Apple -> "/images/cells/apple.png"
  Box -> "/images/cells/box.png"