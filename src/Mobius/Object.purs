module Mobius.Object where

import Prelude

data Object
  = Wall
  | Apple
  | Box

data ObjectProperties = Push | Stop

derive instance Eq Object
derive instance Eq ObjectProperties

canPush :: Object -> ObjectProperties
canPush = case _ of
  Wall -> Stop
  Apple -> Push
  Box -> Push