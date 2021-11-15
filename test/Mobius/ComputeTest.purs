module Test.Mobius.ComputeTest where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple.Nested ((/\))
import Matrix (Matrix, empty, fromArray, set, zipWith)
import Mobius.Game.Directions (Directions(..))
import Mobius.Game.LatticePoint (LatticePoint(..))
import Mobius.Game.Map2D (Cell(..), Map2D(..), WithSingularPoint(..), compute)
import Mobius.Game.MapState (MapEnv(..), MapState, makeMapState)
import Mobius.Game.Object (Object(..))
import Mobius.Game.Surface (Surface(..))

e :: forall t3. Cell t3
e = Empty

a :: Cell Object
a = Object Apple

b :: Cell Object
b = Object Box

w :: Cell Object
w = Object Wall

testFrontMap :: Matrix (Cell Object)
testFrontMap = fromMaybe empty $ fromArray
  [ [ e, e, e, e, e ]
  , [ e, e, e, e, e ]
  , [ e, e, e, e, e ]
  , [ e, a, e, e, e ]
  , [ e, e, e, e, e ]
  ]

testBackMap :: Matrix (Cell Object)
testBackMap = fromMaybe empty $ fromArray
  [ [ e, e, e, e, e ]
  , [ e, e, e, e, e ]
  , [ e, e, e, e, e ]
  , [ e, e, e, e, e ]
  , [ e, e, e, e, e ]
  ]

testDestinations :: forall a. Map2D a
testDestinations = Map2D $ empty

testMap2D :: Map2D (Cell Object)
testMap2D = Map2D $ fromMaybe empty $ set 2 2 SingularPoint
  =<< zipWith (\x y -> NotSingularPoint (x /\ y)) testFrontMap testBackMap

testWalk :: Array Directions
testWalk =
  [ Down
  , Down
  , Down
  , Right
  , Right
  , Right
  , Right
  ]

testMapState :: MapState
testMapState = makeMapState $ MapEnv
  { character: LatticePoint Front 0 0
  , destinations: testDestinations
  , map2D: testMap2D
  }

testCompute1 :: LatticePoint -> Directions -> Maybe LatticePoint
testCompute1 = compute testMap2D
