module WebApp.Components.Common where

import Prelude

import Halogen.HTML.Properties (IProp, class_)
import Web.HTML.Common (ClassName(..))
import Data.Array (singleton)
import Data.Foldable (intercalate)
import Data.String as String
import Halogen.HTML as HH

css :: forall r i. String -> IProp (class :: String | r) i
css str = class_ $ ClassName str

makeText :: forall w i. String -> Array (HH.HTML w i)
makeText str = intercalate (singleton HH.br_) $ map (HH.text >>> singleton) $ String.split (String.Pattern "\n") str