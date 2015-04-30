module CPU where

import Bitwise exposing (..)
import List
import Maybe   exposing (..)
import Util    exposing (oct)

type Register = R0
              | R1
              | R2
              | R3
              | R4
              | R5
              | SP
              | PC

register n = case n of
             0 -> R0
             1 -> R1
             2 -> R2
             3 -> R3
             4 -> R4
             5 -> R5
             6 -> SP
             7 -> PC

type Method = Plain     Register
            | Increment Register
            | Decrement Register
            | Index     Register

method bc = let kind = bc `shiftRight` 4
                reg  = register (bc `and` (oct 7))
             in case kind of
                0 -> Plain reg
                1 -> Increment reg
                2 -> Decrement reg
                3 -> Index reg

type Operand = Direct   Method
             | Indirect Method

operand bc = let kind = (bc `shiftRight` 3) `and` 1
                 meth = method bc
              in case kind of
                 0 -> Direct meth
                 1 -> Indirect meth


type Code = HALT
          | WAIT
          | RTI
          | RTS Register
          | JMP Operand
          | JSR Register Operand
          | MOV Operand  Operand

opvoid c = if | c == oct 0 -> Just HALT
              | c == oct 1 -> Just WAIT
              | c == oct 2 -> Just RTI
              | otherwise  -> Nothing

opR c = let o = c `and` (oct 177770)
            r = register (c `and` (oct 7))
    in if | o == oct 200 -> Just (RTS r)
          | otherwise    -> Nothing

opDD c = let o = c `and` (oct 177700)
             d = operand (c `and` (oct 77))
     in if | o == oct 100 -> Just (JMP d)
           | otherwise    -> Nothing

opRDD c = let o = c `and` (oct 177000)
              r = register ((c `shiftRight` 6) `and` (oct 7))
              d = operand (c `and` (oct 77))
      in if | o == oct 4000 -> Just (JSR r d)
            | otherwise     -> Nothing

opSSDD c = let o = c `and` (oct 170000)
               s = operand ((c `shiftRight` 3) `and` (oct 77))
               d = operand (c `and` (oct 77))
       in if | o == oct 10000 -> Just (MOV s d)
             | otherwise      -> Nothing

dispatch c = oneOf (List.map (\f -> f c) [ opSSDD, opRDD, opDD, opR, opvoid ])
