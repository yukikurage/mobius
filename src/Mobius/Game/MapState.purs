module Mobius.Game.MapState where

import Prelude

import Common (upRange)
import Data.Array (cons, uncons)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Data.Tuple.Nested ((/\))
import Graphics.Canvas (TextBaseline(..), closePath, lineTo, moveTo, setFont, setTextBaseline, strokePath, translate)
import Mobius.Game.Common (drawImagesFromImages, resetTransForm)
import Mobius.Game.Directions (Directions)
import Mobius.Game.Drawable (class Drawable, draw)
import Mobius.Game.LatticePoint (LatticePoint(..), computeWithPoint, toPoint)
import Mobius.Game.Map2D (Cell(..), Map2D, WithSingularPoint(..), compute, heightMap2D, index, indexWithPoint, moveSingularPoint, updateAt, widthMap2D)
import Mobius.Game.Object (Object, ObjectProperty(..), objectProperty)
import Mobius.Game.Surface (Surface(..))

data MapEnv = MapEnv
  { map2D :: Map2D (Cell Object)
  , character :: LatticePoint
  , destinations :: Map2D Boolean
  }

data MapState = MapState { mapEnv :: MapEnv, history :: Array MapEnv, initMap :: MapEnv }

data Input = Move Directions | Z | R

makeMapState :: MapEnv -> MapState
makeMapState env = MapState { mapEnv: env, history: [], initMap: env }

updateState :: MapState -> Input -> MapState
updateState (MapState { mapEnv, history, initMap }) i = case i of
  Move d -> MapState { mapEnv: move mapEnv d, history: cons mapEnv history, initMap }
  Z -> case uncons history of
    Just { head, tail } -> MapState { mapEnv: head, history: tail, initMap }
    Nothing -> MapState { mapEnv, history, initMap }
  R -> makeMapState initMap

instance Show MapEnv where
  show (MapEnv { map2D, character, destinations }) = "MapEnv{\n"
    <> show map2D
    <> ",\ncharacter:"
    <> show character
    <> ",\ndestinations:\n"
    <> show destinations
    <> "\n}"

move :: MapEnv -> Directions -> MapEnv
move (MapEnv { map2D, character, destinations }) d = MapEnv $ case compute map2D character d of
  Nothing -> { map2D, character, destinations } --今いる場所が特異点ならすすめない
  Just nextPos -> case index map2D nextPos of --目の前が
    Just SingularPoint -> case indexWithPoint map2D doubleNextPoint of --特異点ならばさらにその前が
      Just (NotSingularPoint (Empty /\ Empty)) -> { map2D: newMap2D, character: nextPos, destinations } --両面になにも無いならすすめる
        where
        newMap2D = moveSingularPoint nextPoint d map2D
      _ -> { map2D, character, destinations } --それ以外なら進めない
      where
      nextPoint = toPoint nextPos
      doubleNextPoint = computeWithPoint nextPoint d
    Just (NotSingularPoint Empty) ->
      { map2D, character: nextPos, destinations } --何も無いならすすめる
    Just (NotSingularPoint (Object x)) -> case objectProperty x of
      Stop -> { map2D, character, destinations } --Stopがあるならすすめない
      Push -> case compute map2D nextPos d of --Pushがあるなら
        Nothing -> { map2D, character, destinations } --その場所が特異点ならすすめない(そんなことはない)
        Just doubleNextPos -> case index map2D doubleNextPos of --そのさらに前が
          Just (NotSingularPoint Empty) -> { map2D: newMap2D, character: nextPos, destinations } --何も無いならすすめる
            where
            newMap2D = updateAt doubleNextPos (Object x)
              $ updateAt nextPos Empty map2D -- 次のMap2Dの状態
          _ -> { map2D, character, destinations } --それ以外ならすすめない
    Nothing -> { map2D, character, destinations } --範囲外ならすすめない

instance Drawable MapState where
  draw ctx images (MapState { mapEnv: (MapEnv { map2D, character }) }) = do
    let
      h = heightMap2D map2D
      w = widthMap2D map2D

      cellSize = 32.0
      backStartHeight = (toNumber h + 1.0) * cellSize
    setFont ctx "20px serif"
    setTextBaseline ctx BaselineTop
    for_ (upRange 0 (h - 1)) \i -> for_ (upRange 0 (w - 1)) \j -> do
      translate ctx { translateX: toNumber j * cellSize, translateY: toNumber i * cellSize }
      case index map2D (LatticePoint Front i j) of
        Just x -> draw ctx images x
        Nothing -> pure unit
      resetTransForm ctx

      translate ctx { translateX: toNumber j * cellSize, translateY: backStartHeight + toNumber i * cellSize }
      case index map2D (LatticePoint Back i j) of
        Just x -> draw ctx images x
        Nothing -> pure unit
      resetTransForm ctx
    strokePath ctx $ do
      moveTo ctx 0.0 backStartHeight
      lineTo ctx 1000.0 backStartHeight
      closePath ctx
    case character of
      LatticePoint Front i j -> do
        translate ctx { translateX: toNumber j * cellSize, translateY: toNumber i * cellSize }
        drawImagesFromImages ctx images "player"
        resetTransForm ctx
      LatticePoint Back i j -> do
        translate ctx { translateX: toNumber j * cellSize, translateY: backStartHeight + toNumber i * cellSize }
        drawImagesFromImages ctx images "player"
        resetTransForm ctx
    pure unit