module CPU where

import Array   exposing (Array)
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

type alias Bus = Array Int

type alias State = {
    r0 : Int
  , r1 : Int
  , r2 : Int
  , r3 : Int
  , r4 : Int
  , r5 : Int
  , sp : Int
  , pc : Int
  , bus : Bus }

read : Register -> State -> Int
read r s = case r of
           R0 -> s.r0
           R1 -> s.r1
           R2 -> s.r2
           R3 -> s.r3
           R4 -> s.r4
           R5 -> s.r5
           SP -> s.sp
           PC -> s.pc

load : Register -> Int -> State -> State
load r v s = let vm = v `and` (oct 177777)
          in case r of
             R0 -> { s | r0 <- vm }
             R1 -> { s | r1 <- vm }
             R2 -> { s | r2 <- vm }
             R3 -> { s | r3 <- vm }
             R4 -> { s | r4 <- vm }
             R5 -> { s | r5 <- vm }
             SP -> { s | sp <- vm }
             PC -> { s | pc <- vm }

inc : Register -> Int -> State -> State
inc r v s = load r (v + read r s) s

get : Int -> State -> (Int, State)
get a s = let v = Array.get a s.bus in case v of
            Just x -> (x, s)

put : Int -> Int -> State -> State
put a v s = { s | bus <-Array.set a v s.bus }


type Accessor = Register Register
              | Address Int

mproc : Int -> Method -> State -> (Accessor, State)
mproc i m s = case m of
           Plain     reg -> (Register reg, s)
           Increment reg -> (Address (read reg s), inc reg i s)
           Decrement reg -> let s1 = inc reg -i s in (Address (read reg s1), s1)
           Index     reg -> let (bs, s1) = get (read PC s) s
                         in (Address ((bs + read reg s1) `and` (oct 177777)), inc PC 2 s1)

proc : Int -> Operand -> State -> (Accessor, State)
proc i o s = case o of
          Direct   m -> mproc i m s
          Indirect m -> let (a, s1) = mproc i m s in case a of
                        Register reg -> (Address (read reg s1), s1)
                        Address  adr -> let (bs, s2) = get adr s1 in
                                        (Address bs, s2)

pbyte = proc 1
pword = proc 2

cperform : Code -> State -> State
cperform c s = case c of
  JMP dest -> let (a, s1) = pword dest s in case a of
              Address adr -> load PC adr s1
