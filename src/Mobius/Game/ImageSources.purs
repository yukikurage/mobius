module Mobius.Game.Assets where

import Data.Tuple.Nested (type (/\), (/\))

imageSources :: Array (String /\ String)
imageSources =
  [ "apple" /\ "./images/cells/apple.png"
  , "wall" /\ "./images/cells/wall.png"
  , "mobius" /\ "./images/cells/mobius.png"
  , "box" /\ "./images/cells/box.png"
  , "destination" /\ "./images/cells/destination.png"
  , "player" /\ "./images/cells/player.png"
  ]