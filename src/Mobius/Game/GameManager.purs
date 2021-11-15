module Mobius.Game.GameManager where

import Prelude

import Data.Array (uncons)
import Data.Int (toNumber)
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, runAff_)
import Effect.Class (liftEffect)
import Effect.Ref (new, read, write)
import Graphics.Canvas (CanvasElement, CanvasImageSource, Context2D, clearRect, getContext2D, setCanvasHeight, setCanvasWidth, tryLoadImage)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent (key)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

data KeyState = KeyDown | KeyPress | KeyUp
data KeyInput = KeyInput String KeyState

type Game gameState =
  { images :: Array (String /\ String) --'名前' /\ 'URL'
  , initState :: gameState
  , fps :: Int
  , render :: Context2D -> (String -> Maybe CanvasImageSource) -> gameState -> Effect Unit
  --, update :: Number -> gameState -> gameState
  , keyHandler :: KeyInput -> gameState -> gameState
  , canvas :: CanvasElement
  , height :: Number
  , width :: Number
  }

runGame :: forall gameState. Game gameState -> Effect Unit
runGame { images, initState, fps, render, keyHandler, canvas, height, width } = loadAndRun images empty
  where
  loadAndRun :: Array (String /\ String) -> Map String CanvasImageSource -> Effect Unit
  loadAndRun xs imageSources = case uncons xs of
    Just { head, tail } -> do
      let
        f Nothing = pure unit
        f (Just x) = loadAndRun tail $ insert (fst head) x $ imageSources
      tryLoadImage (snd head) f
    Nothing -> do
      setCanvasHeight canvas height
      setCanvasWidth canvas width
      ctx <- getContext2D canvas
      gameStateRef <- new initState
      runAff_ (\_ -> pure unit) $ mainLoop ctx imageSources gameStateRef
      keyEventRegister gameStateRef

  keyEventRegister gameStateRef = do
    w <- window
    let
      makeHandler x = eventListener \e -> case KeyboardEvent.fromEvent e of
        Nothing -> pure unit
        Just keyE -> do
          gameState <- liftEffect $ read gameStateRef
          let
            newGameState = keyHandler (KeyInput (key keyE) x) gameState
          write newGameState gameStateRef
    keyDownHandler <- makeHandler KeyDown
    keyPressHandler <- makeHandler KeyPress
    keyUpHandler <- makeHandler KeyUp
    addEventListener (EventType "keydown") keyDownHandler false $ Window.toEventTarget w
    addEventListener (EventType "keypress") keyPressHandler false $ Window.toEventTarget w
    addEventListener (EventType "keyup") keyUpHandler false $ Window.toEventTarget w

  mainLoop ctx imageSources gameStateRef = do
    gameState <- liftEffect $ read gameStateRef
    delay $ Milliseconds $ 1000.0 / toNumber fps
    liftEffect $ clearRect ctx { x: 0.0, y: 0.0, height, width }
    liftEffect $ render ctx (\s -> lookup s imageSources) gameState
    mainLoop ctx imageSources gameStateRef
