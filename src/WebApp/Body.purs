module WebApp.Body where

import Prelude (($))
import Halogen.HTML (div_)
import Halogen.Hooks as Hooks

component = Hooks.component \_ _ -> Hooks.pure $ div_ []