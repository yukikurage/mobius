module Mobius.Surface where

import Prelude

data Surface = Front | Back

derive instance Eq Surface

instance Show Surface where
  show Front = "Front"
  show Back = "Back"