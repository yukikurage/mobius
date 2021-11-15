module Mobius.Game.Common where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas (CanvasImageSource, Context2D, clearRect, drawImage, fill, rect, setTransform)
import Mobius.Game.GameSettings (canvasHeight, canvasWidth)

drawImagesFromImages :: Context2D -> (String -> Maybe CanvasImageSource) -> String -> Effect Unit
drawImagesFromImages ctx f str = case f str of
  Just img -> drawImage ctx img 0.0 0.0
  Nothing -> pure unit

resetTransForm :: Context2D -> Effect Unit
resetTransForm ctx = setTransform ctx { m11: 1.0, m12: 0.0, m21: 0.0, m22: 1.0, m31: 0.0, m32: 0.0 }

clearCanvas :: Context2D -> Effect Unit
clearCanvas ctx = clearRect ctx { x: 0.0, y: 0.0, height: canvasHeight, width: canvasWidth }

fillCanvas :: Context2D -> Effect Unit
fillCanvas ctx = do
  rect ctx { x: 0.0, y: 0.0, height: canvasHeight, width: canvasWidth }
  fill ctx