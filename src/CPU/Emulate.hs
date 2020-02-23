
module CPU.Emulate where

import Control.Lens
import CPU.CPU
import Data.Bits

-- TODO: Lägga till test-case som kan kolla mot fontset:ens position i minnet till exempel, 
--       eftersom den alltid är samma.
{- fetchOpcode cpu
   Reads an opcode from the memory of a CPU.

   PRE: (pc cpu) does not point outside the size of memory.
   RETURNS: the four hexadecimal values that make up the two following bytes that (pc cpu) points to
            in (memory cpu).
   EXAMPLES: fetchOpcode ((pc cpu) points to [..., 0x12, 0x34, ..]) = (0x1, 0x2, 0x3, 0x4)
             fetchOpcode ((pc cpu) points to [..., 0xA3, 0xD8, ..]) = (0xA, 0x3, 0xD, 0x8)
-}
fetchOpcode :: CPU -> Opcode
fetchOpcode cpu = (a1, a2, b1, b2)
                  where
                    a1 = shift ((.&.) byte1 0xF0) (-4)
                    a2 = (.&.) byte1 0xF
                    b1 = shift ((.&.) byte2 0xF0) (-4)
                    b2 = (.&.) byte2 0xF
                    byte1 = memory cpu !! pc cpu
                    byte2 = memory cpu !! pc cpu + 1

{- executeOpcode cpu opcode
   Executes a given opcode and alters the state of the CPU it was executed on.
   
   PRE: opcode is a valid opcode and matches any of the cases defined in
        the function.
   RETURNS: cpu where opcode has been executed and altered the state of cpu in some way.
   EXAMPLES: TODO
-}
executeOpcode :: CPU -> Opcode -> CPU
executeOpcode cpu opcode =
  case opcode of 
    (0x0, 0x0, 0xE, 0x0) ->
      nextPC $ clearScreen cpu
    (0x0, 0x0, 0xE, 0xE) ->
      nextPC $ returnFromSubroutine cpu
    (0x1, _, _, _) ->
      jumpToAddress cpu $ opNNN opcode
    (0x2, _, _, _) ->
      callSubroutine cpu $ opNNN opcode
    (0x3, x, _, _) ->
      nextPC $ skipInstructionIf cpu (v cpu !! x == opNN opcode)
    (0x4, x, _, _) ->
      nextPC $ skipInstructionIf cpu (v cpu !! x /= opNN opcode)
    (0x5, x, y, 0x0) ->
      nextPC $ skipInstructionIf cpu (v cpu !! x == v cpu !! y)
    (0x6, x, _, _) ->
      nextPC $ setRegister cpu x (opNN opcode)
    (0x7, x, _, _) ->
      nextPC $ setRegister cpu x (v cpu !! x + opNN opcode)
    (0x8, x, y, 0x0) ->
      nextPC $ setRegister cpu x (v cpu !! y)
    (0x8, x, y, 0x1) ->
      nextPC $ setRegister cpu x (v cpu !! x .|. v cpu !! y)
    (0x8, x, y, 0x2) ->
      nextPC $ setRegister cpu x (v cpu !! x .&. v cpu !! y)
    (0x8, x, y, 0x3) ->
      nextPC $ setRegister cpu x (xor (v cpu !! x) (v cpu !! y))
    (0x8, x, y, 0x4) ->
      nextPC $ setRegister cpu x (v cpu !! x + v cpu !! y)
    (0x8, x, y, 0x5) ->
      nextPC $ setRegister cpu x (v cpu !! x - v cpu !! y)
    (0x8, x, y, 0x6) ->
      nextPC $ setRegister ucpu x (shiftR (v cpu !! x) 1)
        where
          ucpu = setRegister cpu 0xF (v cpu !! x .&. 0x1)
    (0x8, x, y, 0x7) ->
      nextPC $ setRegister ucpu x (v cpu !! y - v cpu !! x)
        where
          ucpu = setRegister cpu 0xF (if v cpu !! y > v cpu !! x then 1 else 0)
    (0x8, x, y, 0xE) ->
      nextPC $ setRegister ucpu x (shiftL (v cpu !! x) 1)
        where
          ucpu = setRegister cpu 0xF (shiftR (v cpu !! x) 7)
    (0x9, x, y, 0x0) ->
      nextPC $ skipInstructionIf cpu (v cpu !! x /= v cpu !! y)
    (0xA, _, _, _) ->
      nextPC $ cpu {i = opNNN opcode}
    (0xB, _, _, _) ->
      nextPC $ jumpToAddress cpu (v cpu !! 0 + opNNN opcode)

nextPC :: CPU -> CPU
nextPC cpu = jumpToAddress cpu (pc cpu + 2)

skipPC :: CPU -> CPU
skipPC cpu = jumpToAddress cpu (pc cpu + 4)

jumpToAddress :: CPU -> Int -> CPU
jumpToAddress cpu addr = cpu {pc = addr}

skipInstructionIf :: CPU -> Bool -> CPU
skipInstructionIf cpu pred | pred      = nextPC cpu
                           | otherwise = cpu

setRegister :: CPU -> Int -> Int -> CPU
setRegister cpu x val = cpu {v = (element x .~ val) (v cpu)}

opNNN :: Opcode -> Int
opNNN (_, x, y, z) = shift x 8 + shift y 4 + z

opNN :: Opcode -> Int
opNN (_, _, x, y) = shift x 4 + y

clearScreen :: CPU -> CPU
clearScreen = undefined

callSubroutine :: CPU -> Int -> CPU
callSubroutine = undefined

returnFromSubroutine :: CPU -> CPU
returnFromSubroutine = undefined