
module CPU.Emulate where

import Control.Lens
import CPU.CPU
import Data.Bits
import System.Random

{- Represents an instruction for the CHIP-8.
   Opcode is a two-byte value that is stored in big-endian format.
   Each Integer in Opcode is a hexadecimal value between 0-F.
   The two first Integers in Opcode corresponds to the first byte of the instruction, 
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
      returnFromSubroutine cpu
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
      nextPC $ jumpToAddress cpu (head (v cpu) + opNNN opcode)
    (0xC, x, _, _) ->
      nextPC $ setRegister ucpu x (randomValue .&. opNN opcode)
        where
          (randomValue, newStdGen) = randomR (0, 255) (rgen cpu)
          ucpu = cpu {rgen = newStdGen}
    (0xD, x, y, n) ->
      nextPC $ drawSprite cpu x y n
    (0xE, x, 0x9, 0xE) ->
      nextPC $ skipInstructionIf cpu (keyboard cpu !! (v cpu !! x))
    (0xE, x, 0xA, 0x1) ->
      nextPC $ skipInstructionIf cpu (not (keyboard cpu !! (v cpu !! x)))
    (0xF, x, 0x0, 0x7) ->
      nextPC $ setRegister cpu x (delay_timer cpu)
    (0xF, x, 0x0, 0xA) ->
      -- TODO
      cpu
    (0xF, x, 0x1, 0x5) ->
      nextPC $ cpu {delay_timer = v cpu !! x}
    (0xF, x, 0x1, 0x8) ->
      nextPC $ cpu {sound_timer = v cpu !! x}
    (0xF, x, 0x1, 0xE) ->
      nextPC $ ucpu {i = i cpu + v cpu !! x}
        where
          ucpu = setRegister cpu 0xF (if i cpu > x then 1 else 0)
    (0xF, x, 0x2, 0x9) ->
      -- Sets I to the location of the sprite for the character in VX. 
      -- Characters 0-F (in hexadecimal) are represented by a 4x5 font. 
      nextPC $ cpu {i = v cpu !! x * 5}
    (0xF, x, 0x3, 0x3) ->
      nextPC $ storeBinaryRepresentation cpu x
    (0xF, x, 0x5, 0x5) ->
      nextPC $ storeRegisters cpu x
    (0xF, x, 0x6, 0x5) ->
      nextPC $ loadRegisters cpu x

nextPC :: CPU -> CPU
nextPC cpu = jumpToAddress cpu (pc cpu + 2)

skipPC :: CPU -> CPU
skipPC cpu = jumpToAddress cpu (pc cpu + 4)

jumpToAddress :: CPU -> Int -> CPU
jumpToAddress cpu addr = cpu {pc = addr}

skipInstructionIf :: CPU -> Bool -> CPU
skipInstructionIf cpu pred | pred      = nextPC cpu
                           | otherwise = cpu

-- Clears the screen by replacing the vram with an empty copy.
clearScreen :: CPU -> CPU
clearScreen cpu = cpu {vram = defaultVRAM}

-- Calls a subroutine by inserting the current position into the stack and jumping to
-- a specific address.
callSubroutine :: CPU -> Int -> CPU
callSubroutine cpu addr = jumpToAddress ucpu addr
  where ucpu = insertToStack cpu (pc (nextPC cpu))

-- Returns from a subroutine by decrementing the stack pointer by 1 and jumping to the stored location
-- in (stack cpu !! sp cpu).
returnFromSubroutine :: CPU -> CPU
returnFromSubroutine cpu = jumpToAddress ucpu (stack ucpu !! sp ucpu)
  where ucpu = cpu {sp = sp cpu - 1}

-- Inserts a given value into the stack where the stack pointer points to,
-- and increases the stack pointer by 1.
insertToStack :: CPU -> Int -> CPU
insertToStack cpu val = ucpu {sp = sp cpu + 1}
  where ucpu = cpu {stack = replace (sp cpu) val (stack cpu)}

-- Sets the register at idx to val.
setRegister :: CPU -> Int -> Int -> CPU
setRegister cpu idx val = cpu {v = replace idx val (v cpu)}

-- Sets the memory position at pos to val.
setMemory :: CPU -> Int -> Int -> CPU
setMemory cpu pos val = cpu {memory = replace pos val (memory cpu)}

-- Stores the binary representation of the value located in (v cpu) at index idx to the memory.
storeBinaryRepresentation :: CPU -> Int -> CPU
storeBinaryRepresentation cpu vidx = setMemory ucpu2 (i cpu) (v cpu !! vidx `div` 100)
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

{- replace idx val list
   Replaces a value at a given index in a list with another value.

   PRE: 0 <= idx <= length list
   RETURNS: list where the value at index idx has been replaced with val.
   EXAMPLES: replace 0 3 [1,2,3] = [3,2,3]
             replace 1 3 [1,1,1] = [1,3,1]
-}
replace :: Int -> a -> [a] -> [a]
replace idx val = element idx .~ val

{- nestedListIndexes rows cols
   Calculates all possible indexes from a pair of rows and cols.

   RETURNS: [(x,y) | x <- [0..cols], y <- [0..rows]]
   EXAMPLES: nestedListIndexes 1 1 = [(0,0),(0,1),(1,0),(1,1)]
             nestedListIndexes 1 2 = [(0,0),(0,1),(1,0),(1,1),(2,0),(2,1)]
-}
nestedListIndexes :: Int -> Int -> [(Int, Int)]
nestedListIndexes rows cols = [(col, row) | col <- [0..cols], row <- [0..rows]]

drawSprite :: CPU -> Int -> Int -> Int -> CPU
drawSprite cpu x y times = drawSprite' (nestedListIndexes times 8) cpu x y
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
                                    newRow = replace x (xor oldVal val) (vram cpu !! y) 
                                  in 
                                    replace y newRow (vram cpu)}

opNNN :: Opcode -> Int
opNNN (_, x, y, z) = shift x 8 + shift y 4 + z

opNN :: Opcode -> Int
opNN (_, _, x, y) = shift x 4 + y