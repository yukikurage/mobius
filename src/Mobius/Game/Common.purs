module Mobius.Game.Common where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas (CanvasImageSource, Context2D, drawImage, setTransform)

drawImagesFromImages :: Context2D -> (String -> Maybe CanvasImageSource) -> String -> Effect Unit
drawImagesFromImages ctx f str = case f str of
  Just img -> drawImage ctx img 0.0 0.0
  Nothing -> pure unit

resetTransForm :: Context2D -> Effect Unit
resetTransForm ctx = setTransform ctx { m11: 1.0, m12: 0.0, m21: 0.0, m22: 1.0, m31: 0.0, m32: 0.0 }