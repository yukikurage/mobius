module WebApp.Components.Common where

import Prelude

import Halogen.HTML.Properties (IProp, class_)
import Web.HTML.Common (ClassName(..))

css :: forall r i. String -> IProp (class :: String | r) i
css str = class_ $ ClassName str