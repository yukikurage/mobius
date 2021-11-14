module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.Mobius.ComputeTest as ComputeTest

main :: Effect Unit
main = do
  log "🍝"
  log "You should add some tests."
  ComputeTest.test
