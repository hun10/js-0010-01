module Video where

import Array
import Bitwise exposing (and, shiftRight)
import Signal exposing (..)
import Time

import Monit10 exposing (fromDecOct)

type alias Devices = {
    ram : Array.Array Int
  , vram : Array.Array Int
  , rom10 : Array.Array Int
  }

devices = {
    ram   = Array.initialize (64 * 256) powerOnPattern
  , vram  = Array.initialize (64 * 256) powerOnPattern
  , rom10 = Array.fromList Monit10.words
  }

read : Devices -> Int -> Maybe Int
read dev address =
  let get = (\mask -> Array.get ((address `and` mask) `shiftRight` 1)) in
  if | address < 0                 -> Nothing
     | address < fromDecOct 40000  -> get (fromDecOct 37777) dev.ram
     | address < fromDecOct 100000 -> get (fromDecOct 37777) dev.vram
     | address < fromDecOct 120000 -> get (fromDecOct 17777) dev.rom10
     | otherwise                   -> Nothing

write : Devices -> Int -> Int -> Devices
write dev address value =
  let set = (\mask -> Array.set ((address `and` mask) `shiftRight` 1) value) in
  if | address < 0                 -> dev
     | address < fromDecOct 40000  -> { dev | ram <- set (fromDecOct 37777) dev.ram }
     | address < fromDecOct 100000 -> { dev | vram <- set (fromDecOct 37777) dev.vram }
     | address < fromDecOct 120000 -> dev
     | otherwise                   -> dev

computer = {
    reg = Array.fromList [fromDecOct 40000, 0, 0, 0, 0, 0, 0, fromDecOct 100000]
  , pws = 0
  , bus = devices
  }

singleStep c =
  let op = Maybe.withDefault 0 (Array.get 0 c.reg)
  in  { c | reg <- Array.set 0 (op + 2) c.reg
          , bus <- write c.bus op 0 }

st1 e = singleStep <| singleStep e

step clock c =
  st1 c

powerOnPattern : Int -> Int
powerOnPattern n =
  if (n `and` (fromDecOct 200)) `shiftRight` 7 == n % 2
    then 0
    else fromDecOct 177777

clock = Time.timestamp (Time.fps 25)

port screenBuffer : Signal (Array.Array Int)
port screenBuffer = map (\c -> c.bus.vram) (foldp step computer clock)
