module Test.Main where

import Prelude

import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))
import WebApp.Body as Body

main :: Effect Unit
main =
  launchAff_ do
    HA.awaitLoad

    bodyElem <- HA.selectElement (QuerySelector "body")

    traverse_ (runUI Body.component unit) bodyElem