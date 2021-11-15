module Mobius.Game.GameState where

import Prelude

import Data.Maybe (Maybe(..))
import Graphics.Canvas (fillText, setFillStyle)
import Mobius.Contents.Maps (testMapState)
import Mobius.Game.Directions (Directions(..))
import Mobius.Game.Drawable (class Drawable, draw)
import Mobius.Game.GameManager (KeyInput(..), KeyState(..))
import Mobius.Game.MapState (MapState, isClear, makeMapState)
import Mobius.Game.MapState as MapState

-- ゲームの状態を管理します．

data Scene = Map MapState | Menu | Clear MapState

data GameState = GameState Scene

data Input = Move Directions | Z | R -- GameStateへの入力です．

updateState :: GameState -> Input -> GameState
updateState (GameState (Map state)) k = GameState $ if isClear nextMap then Clear nextMap else Map nextMap
  where
  nextMap = case k of
    Move d -> MapState.updateState state $ MapState.Move d
    Z -> MapState.updateState state MapState.Z
    R -> MapState.updateState state MapState.R
updateState x _ = x

strToInput :: String -> Maybe Input
strToInput = case _ of
  "w" -> Just $ Move Up
  "d" -> Just $ Move Right
  "s" -> Just $ Move Down
  "a" -> Just $ Move Left
  "z" -> Just Z
  "r" -> Just R
  _ -> Nothing

keyHandler :: KeyInput -> GameState -> GameState
keyHandler k state = case k of
  KeyInput s KeyDown -> case strToInput s of
    Just x -> updateState state x
    _ -> state
  _ -> state

instance Drawable GameState where
  draw ctx images = case _ of
    GameState Menu -> pure unit
    GameState (Map state) -> draw ctx images state
    GameState (Clear state) -> do
      draw ctx images state
      setFillStyle ctx "#999"
      fillText ctx "Clear!" 20.0 20.0

initGameState :: GameState
initGameState = GameState $ Map $ makeMapState testMapState