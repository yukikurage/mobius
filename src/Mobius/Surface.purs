module Mobius.Surface where

import Prelude

data Surface = Front | Back

changeSurface :: Surface -> Surface
changeSurface Front = Back
changeSurface Back = Front

derive instance Eq Surface

instance Show Surface where
  show Front = "Front"
  show Back = "Back"