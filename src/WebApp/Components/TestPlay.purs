module WebApp.Components.TestPlay (component) where

import Prelude hiding (div)

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (logShow)
import Halogen (Component, lift)
import Halogen.HTML (button)
import Halogen.HTML.Events (onKeyDown)
import Halogen.Hooks as Hooks
import Matrix (Matrix, empty, fromArray, set, zipWith)
import Mobius.Directions (Directions(..))
import Mobius.LatticePoint (LatticePoint(..))
import Mobius.Map2D (Cell(..), Map2D(..), WithSingularPoint(..))
import Mobius.MapState (MapState(..), move)
import Mobius.Object (Object(..))
import Mobius.Surface (Surface(..))
import Web.UIEvent.KeyboardEvent (key)
import WebApp.Components.Common (css, makeText)

component :: forall q i o m. Monad m => MonadEffect m => Component q i o m
component = Hooks.component \_ _ -> Hooks.do
  mapState /\ mapStateId <- Hooks.useState testMapState

  let
    keyDownHandler e = do
      let
        dir = case key e of
          "w" -> Just Up
          "d" -> Just Right
          "s" -> Just Down
          "a" -> Just Left
          _ -> Nothing
        newMapState = fromMaybe mapState $ move mapState <$> dir
      lift $ spy "X" $ logShow newMapState
      Hooks.put mapStateId newMapState
      pure unit

  Hooks.pure $ button [ css "h-screen w-screen m-9", onKeyDown keyDownHandler ] $ makeText $ show mapState

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

testMapState :: MapState
testMapState = MapState
  { character: LatticePoint Front 0 0
  , destinations: testDestinations
  , map2D: testMap2D
  }