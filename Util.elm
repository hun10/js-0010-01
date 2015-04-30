module Util where

oct : Int -> Int
oct n = if n == 0 then 0 else n % 10 + 8 * (oct (n // 10))
