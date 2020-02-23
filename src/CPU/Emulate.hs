
module CPU.Emulate where

import CPU.CPU
import Data.Bits

{- fetchOpcode cpu
   Reads an opcode from the memory of a CPU.

   PRE: (pc cpu) does not point outside of memory.
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