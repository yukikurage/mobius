module Mobius.Game.Drawable where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Graphics.Canvas (CanvasImageSource, Context2D)

type Images = String -> Maybe CanvasImageSource

class Drawable a where
  draw :: Context2D -> Images -> a -> Effect Unit