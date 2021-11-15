module Mobius.Contents.Maps where

import Prelude hiding (div)

import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Matrix (empty, fromArray)
import Mobius.Game.LatticePoint (LatticePoint(..))
import Mobius.Game.Map2D (Cell(..), Map2D(..), WithSingularPoint(..))
import Mobius.Game.MapState (MapEnv(..))
import Mobius.Game.Object (Object(..))
import Mobius.Game.Surface (Surface(..))

e :: forall a. Cell a
e = Empty

a :: Cell Object
a = Object Apple

b :: Cell Object
b = Object Box

w :: Cell Object
w = Object Wall

singu :: forall a. WithSingularPoint a
singu = SingularPoint

makeDoubledCell :: forall a b. a -> b -> WithSingularPoint (Tuple a b)
makeDoubledCell x y = NotSingularPoint $ x /\ y

infix 5 makeDoubledCell as ~

testMapState :: MapEnv
testMapState = MapEnv
  { character: LatticePoint Front 6 3
  , destinations: Map2D $ empty
  , map2D: Map2D $ fromMaybe empty $ fromArray
      [ [ w ~ e, w ~ e, e ~ e, e ~ e, e ~ e, e ~ e, e ~ e ]
      , [ w ~ e, e ~ e, e ~ e, e ~ e, e ~ e, e ~ e, e ~ e ]
      , [ e ~ e, e ~ e, e ~ e, e ~ b, e ~ e, e ~ e, e ~ e ]
      , [ e ~ e, e ~ e, w ~ a, singu, a ~ w, e ~ e, e ~ e ]
      , [ e ~ e, e ~ e, e ~ e, b ~ e, e ~ e, e ~ e, e ~ e ]
      , [ e ~ e, e ~ e, e ~ e, e ~ e, e ~ e, e ~ e, e ~ w ]
      , [ e ~ e, e ~ e, e ~ e, e ~ e, e ~ e, e ~ w, e ~ w ]
      ]
  }