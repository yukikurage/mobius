module Mobius.WebApp.Components.Body (component) where

import Prelude hiding (div)

import Effect.Class (class MonadEffect)
import Halogen (Component)
import Halogen.HTML (div_)
import Halogen.Hooks as Hooks

component :: forall q i o m. Monad m => MonadEffect m => Component q i o m
component = Hooks.component \_ _ -> Hooks.do
  Hooks.pure $ div_ []
