module Main where

import Prelude

import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))
import Mobius.WebApp.Components.GameCanvas as GameCanvas

main :: Effect Unit
main =
  launchAff_ do
    HA.awaitLoad

    bodyElem <- HA.selectElement (QuerySelector "body")

    traverse_ (runUI GameCanvas.component unit) bodyElem