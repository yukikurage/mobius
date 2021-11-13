module Test.Mobius.ComputeTest where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (for_)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Ref (new, read, write)
import Matrix (Matrix, empty, fromArray, set, zipWith)
import Mobius.Directions (Directions(..))
import Mobius.LatticePoint (LatticePoint(..))
import Mobius.Map2D (Cell(..), Map2D(..), WithSingularPoint(..), compute)
import Mobius.MapState (MapState(..), move)
import Mobius.Object (Object(..))
import Mobius.Surface (Surface(..))

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
  , [ e, e, e, e, e ]
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
  , Right
  , Right
  , Up
  , Up
  , Left
  , Left
  ]

testMapState :: MapState
testMapState = MapState
  { character: LatticePoint Front 1 1
  , destinations: testDestinations
  , map2d: testMap2D
  }

testCompute1 :: LatticePoint -> Directions -> Maybe LatticePoint
testCompute1 = compute testMap2D

test :: Effect Unit
test = do
  logShow testMapState
  stateRef <- new testMapState
  for_ testWalk \dir -> do
    state <- read stateRef
    let
      newState = move state dir
    logShow $ (\(MapState { character }) -> character) $ newState
    write newState stateRef
  for_ testWalk \dir -> do
    state <- read stateRef
    let
      newState = move state dir
    logShow $ (\(MapState { character }) -> character) $ newState
    write newState stateRef
  pure unit