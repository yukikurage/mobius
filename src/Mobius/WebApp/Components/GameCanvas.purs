module Mobius.WebApp.Components.GameCanvas where

import Prelude hiding (div)

import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Graphics.Canvas (getCanvasElementById)
import Halogen (Component)
import Halogen.HTML (canvas, div)
import Halogen.HTML.Properties (id, style, tabIndex)
import Halogen.Hooks as Hooks
import Mobius.Game.Assets (imageSources)
import Mobius.Game.Drawable (draw)
import Mobius.Game.GameManager (runGame)
import Mobius.Game.GameSettings (canvasHeight, canvasWidth)
import Mobius.Game.GameState (initGameState, keyHandler)
import Mobius.WebApp.Components.Common (css)

component :: forall q i o m. MonadEffect m => Component q i o m
component = Hooks.component \_ _ -> Hooks.do
  Hooks.useLifecycleEffect do
    maybeCanvas <- liftEffect $ getCanvasElementById "canvas"
    case maybeCanvas of
      Just canvas -> liftEffect do
        runGame { canvas, images: imageSources, fps: 30, initState: initGameState, keyHandler: keyHandler, render: draw, height: canvasHeight, width: canvasWidth }
      -- width <- getCanvasWidth canvas
      -- height <- getCanvasHeight canvas
      -- clearRect ctx { x: 0.0, y: 0.0, width, height }
      -- draw ctx img gameState
      Nothing -> pure unit
    pure Nothing

  Hooks.pure $ div [ css "flex items-center justify-center h-screen w-screen p-3 bg-gameBlack" ]
    [ canvas [ tabIndex 0, id "canvas", css "object-contain h-full w-full outline-none bg-gameBlack", style "image-rendering: pixelated;" ]
    ]