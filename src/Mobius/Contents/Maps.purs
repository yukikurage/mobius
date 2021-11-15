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

f = false

t = true

singu :: forall a. WithSingularPoint a
singu = SingularPoint

makeDoubledCell :: forall a b. a -> b -> WithSingularPoint (Tuple a b)
makeDoubledCell x y = NotSingularPoint $ x /\ y

infix 5 makeDoubledCell as ~

testMapState :: MapEnv
testMapState = MapEnv
  { character: LatticePoint Front 5 3
  , destinations: Map2D $ fromMaybe empty $ fromArray
      [ [ f ~ f, f ~ f, f ~ f, f ~ f, f ~ f, f ~ f ]
      , [ f ~ f, f ~ f, f ~ f, f ~ f, f ~ f, f ~ f ]
      , [ f ~ f, f ~ f, f ~ f, f ~ f, f ~ t, f ~ f ]
      , [ f ~ f, f ~ f, singu, f ~ f, f ~ f, f ~ f ]
      , [ f ~ f, f ~ f, f ~ f, f ~ f, f ~ f, f ~ f ]
      , [ f ~ f, f ~ f, f ~ f, f ~ f, f ~ f, f ~ f ]
      ]
  , map2D: Map2D $ fromMaybe empty $ fromArray
      [ [ a ~ e, w ~ w, e ~ e, e ~ w, e ~ e, e ~ e ]
      , [ a ~ e, w ~ w, e ~ e, w ~ w, e ~ e, e ~ e ]
      , [ a ~ e, w ~ e, e ~ a, e ~ e, b ~ e, e ~ e ]
      , [ e ~ e, w ~ w, singu, w ~ w, w ~ e, e ~ e ]
      , [ e ~ e, e ~ e, e ~ w, e ~ e, e ~ e, e ~ e ]
      , [ e ~ e, e ~ e, e ~ e, e ~ e, e ~ e, e ~ e ]
      ]
  }