module Mobius.WebApp.Components.TestPlay (component) where

import Prelude hiding (div)

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen (Component)
import Halogen.HTML (button)
import Halogen.HTML.Events (onKeyDown)
import Halogen.Hooks as Hooks
import Mobius.Contents.Maps (testMapState)
import Mobius.Game.Directions (Directions(..))
import Mobius.Game.MapState (move)
import Mobius.WebApp.Components.Common (css, makeText)
import Web.UIEvent.KeyboardEvent (key)

component :: forall q i o m. Monad m => MonadEffect m => Component q i o m
component = Hooks.component \_ _ -> Hooks.do
  mapState /\ mapStateId <- Hooks.useState testMapState

  let
    keyDownHandler e = do
      let
        dir = case key e of
          "w" -> Just Up
          "d" -> Just Right
          "s" -> Just Down
          "a" -> Just Left
          _ -> Nothing
        newMapState = fromMaybe mapState $ move mapState <$> dir
      Hooks.put mapStateId newMapState
      pure unit

  Hooks.pure $ button [ css "h-screen w-screen m-9", onKeyDown keyDownHandler ] $ makeText $ show mapState