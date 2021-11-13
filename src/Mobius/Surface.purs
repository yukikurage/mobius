module Mobius.Surface where

import Prelude

data Surface = Front | Back

instance Show Surface where
  show Front = "Front"
  show Back = "Back"