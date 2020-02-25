
module CPU.Emulate where

import Control.Lens
import CPU.CPU
import qualified CPU.Utility as Util
import Data.Bits
import System.Random

{- Represents an instruction for the CHIP-8.
   Opcode is a two-byte value that is stored in big-endian format.
   Each Integer in Opcode is a hexadecimal value between 0-F.
   The two first Ints in Opcode corresponds to the first byte of the instruction, 
   and the last two corresponds to the second byte. 

   INVARIANT: Each integer in Opcode is a hexadecimal character, i.e 0-9 or A-F.
              Opcode corresponds to a defined instruction in executeOpcode.
-}
type Opcode = (Int, Int, Int, Int)

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
    (a1, a2) = Util.splitByte $ memory cpu !! pc cpu
    (b1, b2) = Util.splitByte $ memory cpu !! pc cpu + 1

{- executeOpcode cpu opcode
   Executes a given opcode and alters the state of the CPU it was executed on.
   
   RETURNS: cpu where opcode has been executed and altered the state of cpu in some way.
   EXAMPLES: TODO
-}
executeOpcode :: CPU -> Opcode -> CPU
executeOpcode cpu opcode =
  case opcode of 
    (0x0, 0x0, 0xE, 0x0) ->
      incPC $ cpu {vram = defaultVRAM}
    (0x0, 0x0, 0xE, 0xE) ->
      returnFromSubroutine cpu
    (0x1, _, _, _) ->
      jumpToAddress cpu $ opNNN opcode
    (0x2, _, _, _) ->
      callSubroutine cpu $ opNNN opcode
    (0x3, x, _, _) ->
      incPC $ skipInstructionIf cpu (v cpu !! x == opNN opcode)
    (0x4, x, _, _) ->
      incPC $ skipInstructionIf cpu (v cpu !! x /= opNN opcode)
    (0x5, x, y, 0x0) ->
      incPC $ skipInstructionIf cpu (v cpu !! x == v cpu !! y)
    (0x6, x, _, _) ->
      incPC $ setRegister cpu x (opNN opcode)
    (0x7, x, _, _) ->
      incPC $ setRegister cpu x (v cpu !! x + opNN opcode)
    (0x8, x, y, 0x0) ->
      incPC $ setRegister cpu x (v cpu !! y)
    (0x8, x, y, 0x1) ->
      incPC $ setRegister cpu x (v cpu !! x .|. v cpu !! y)
    (0x8, x, y, 0x2) ->
      incPC $ setRegister cpu x (v cpu !! x .&. v cpu !! y)
    (0x8, x, y, 0x3) ->
      incPC $ setRegister cpu x (xor (v cpu !! x) (v cpu !! y))
    (0x8, x, y, 0x4) ->
      incPC $ setRegister cpu x (v cpu !! x + v cpu !! y)
    (0x8, x, y, 0x5) ->
      incPC $ setRegister cpu x (v cpu !! x - v cpu !! y)
    (0x8, x, y, 0x6) ->
      incPC $ setRegister ucpu x (shiftR (v cpu !! x) 1)
        where
          ucpu = setRegister cpu 0xF (v cpu !! x .&. 0x1)
    (0x8, x, y, 0x7) ->
      incPC $ setRegister ucpu x (v cpu !! y - v cpu !! x)
        where
          ucpu = setRegister cpu 0xF (if v cpu !! y > v cpu !! x then 1 else 0)
    (0x8, x, y, 0xE) ->
      incPC $ setRegister ucpu x (shiftL (v cpu !! x) 1)
        where
          ucpu = setRegister cpu 0xF (shiftR (v cpu !! x) 7)
    (0x9, x, y, 0x0) ->
      incPC $ skipInstructionIf cpu (v cpu !! x /= v cpu !! y)
    (0xA, _, _, _) ->
      incPC $ cpu {i = opNNN opcode}
    (0xB, _, _, _) ->
      incPC $ jumpToAddress cpu (head (v cpu) + opNNN opcode)
    (0xC, x, _, _) ->
      incPC $ setRegister ucpu x (randomValue .&. opNN opcode)
        where
          (randomValue, newStdGen) = randomR (0, 255) (rgen cpu)
          ucpu = cpu {rgen = newStdGen}
    (0xD, x, y, n) ->
      incPC $ drawSprite cpu x y n
    (0xE, x, 0x9, 0xE) ->
      incPC $ skipInstructionIf cpu (keyboard cpu !! (v cpu !! x))
    (0xE, x, 0xA, 0x1) ->
      incPC $ skipInstructionIf cpu (not (keyboard cpu !! (v cpu !! x)))
    (0xF, x, 0x0, 0x7) ->
      incPC $ setRegister cpu x (delay_timer cpu)
    (0xF, x, 0x0, 0xA) ->
      -- TODO
      cpu
    (0xF, x, 0x1, 0x5) ->
      incPC $ cpu {delay_timer = v cpu !! x}
    (0xF, x, 0x1, 0x8) ->
      incPC $ cpu {sound_timer = v cpu !! x}
    (0xF, x, 0x1, 0xE) ->
      incPC $ ucpu {i = i cpu + v cpu !! x}
        where
          ucpu = setRegister cpu 0xF (if i cpu > x then 1 else 0)
    (0xF, x, 0x2, 0x9) ->
      incPC $ cpu {i = v cpu !! x * 5}
    (0xF, x, 0x3, 0x3) ->
      incPC $ storeBCDRepresentation cpu x
    (0xF, x, 0x5, 0x5) ->
      incPC $ storeRegisters cpu x
    (0xF, x, 0x6, 0x5) ->
      incPC $ loadRegisters cpu x
    -- If the instruction is not defined we just return the unchanged cpu instead of
    -- throwing an error.
    _ -> cpu

incPC :: CPU -> CPU
incPC cpu = jumpToAddress cpu (pc cpu + 2)

jumpToAddress :: CPU -> Int -> CPU
jumpToAddress cpu addr = cpu {pc = addr}

skipInstructionIf :: CPU -> Bool -> CPU
skipInstructionIf cpu pred | pred      = incPC cpu
                           | otherwise = cpu

-- Calls a subroutine by inserting the current position into the stack and jumping to
-- a specific address.
callSubroutine :: CPU -> Int -> CPU
callSubroutine cpu addr = jumpToAddress ucpu addr
  where ucpu = insertToStack cpu (pc (incPC cpu))

-- Returns from a subroutine by decrementing the stack pointer by 1 and jumping to the stored location
-- in (stack cpu !! sp cpu).
returnFromSubroutine :: CPU -> CPU
returnFromSubroutine cpu = jumpToAddress ucpu (stack ucpu !! sp ucpu)
  where ucpu = cpu {sp = sp cpu - 1}

-- Inserts a given value into the stack where the stack pointer points to,
-- and increases the stack pointer by 1.
insertToStack :: CPU -> Int -> CPU
insertToStack cpu val = ucpu {sp = sp cpu + 1}
  where ucpu = cpu {stack = Util.replace (sp cpu) val (stack cpu)}

-- Sets the register at idx to val.
setRegister :: CPU -> Int -> Int -> CPU
setRegister cpu idx val = cpu {v = Util.replace idx val (v cpu)}

-- Sets the memory position at pos to val.
setMemory :: CPU -> Int -> Int -> CPU
setMemory cpu pos val = cpu {memory = Util.replace pos val (memory cpu)}

-- Stores the binary representation of the value located in (v cpu) at index idx to the memory.
storeBCDRepresentation :: CPU -> Int -> CPU
storeBCDRepresentation cpu vidx = setMemory ucpu2 (i cpu) (v cpu !! vidx `div` 100)
  where
    ucpu2 = setMemory ucpu1 (i cpu + 1) ((v cpu !! vidx `mod` 100) `div` 10)
      where
        ucpu1 = setMemory cpu (i cpu + 2) (v cpu !! vidx `mod` 10)

-- Copies all the values from (v cpu) register to the memory starting at position (i cpu).
storeRegisters :: CPU -> Int -> CPU
storeRegisters cpu idx | idx == 0  = ucpu
                       | otherwise = storeRegisters ucpu (idx - 1)
                         where ucpu = setMemory cpu (i cpu + idx) (v cpu !! idx)

-- Transfers the copied register values from memory back into (v cpu).
loadRegisters :: CPU -> Int -> CPU
loadRegisters cpu idx | idx == 0  = ucpu
                      | otherwise = loadRegisters ucpu (idx - 1)
                        where ucpu = setRegister cpu idx (memory cpu !! i cpu + idx)

drawSprite :: CPU -> Int -> Int -> Int -> CPU
drawSprite cpu x y times = drawSprite' (Util.nestedListIndexes times 8) cpu x y
  where
    drawSprite' [] cpu _ _ = cpu
    drawSprite' ((col, row):xs) cpu x y = let
                                            y2 = (v cpu !! y + row) `mod` windowHeight
                                            x2 = (v cpu !! x + col) `mod` windowWidth
                                            output = shiftR (memory cpu !! (i cpu + row)) (7 - col) .&. 1
                                          in 
                                            drawSprite' xs (xorVram cpu x2 y2 output) x y

xorVram :: CPU -> Int -> Int -> Int -> CPU
xorVram cpu x y val = cpu {vram = let 
                                    oldVal = (vram cpu !! y) !! x
                                    newRow = Util.replace x (xor oldVal val) (vram cpu !! y) 
                                  in 
                                    Util.replace y newRow (vram cpu)}

opNNN :: Opcode -> Int
opNNN (_, x, y, z) = shift x 8 + shift y 4 + z

opNN :: Opcode -> Int
opNN (_, _, x, y) = shift x 4 + y