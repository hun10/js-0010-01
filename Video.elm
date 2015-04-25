module Video where

import Array
import Signal exposing (..)
import Time

generate delta =
  Array.initialize (64 * 256) (\n -> n * floor delta)

port screenBuffer : Signal (Array.Array Int)
port screenBuffer = map generate (Time.fps 50)
