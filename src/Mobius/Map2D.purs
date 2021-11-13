module Mobius.Map2D where

import Mobius.Object (Object)
import Mobius.Surface (Surface)

data Map2D = Map2D (Surface -> Array (Array Object))