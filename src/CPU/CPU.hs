
module CPU.CPU where

type Opcode = (Int, Int, Int, Int)

{- Represents a centralised state for the CPU of the CHIP-8.
   The CHIP-8's state is the inner workings of the computer. It holds all the necessary data
   that it needs to have in order to execute any instructions and to interpret any data.

   INVARIANT: TODO
-}
data CPU = Cpu { v :: [Int]         -- V Register containing 16 8-bit registrars. Index 0, 1, 2 ... E, F.
               , i :: Int           -- 16 bit register for memory address.
               , sound_timer :: Int 
               , delay_timer :: Int 
               , pc :: Int          -- Pointer to memory for current opcode. 0 =< pc =< 4095
               , memory :: [Int]    -- Place to store program data (instructions). 4096 bytes.
               , stack :: [Int]     -- Stack
               , sp :: Int          -- Pointer to current place in the stack.
               , vram :: [[Int]]    -- Memory containing what pixels are to be drawed on the screen.
               , keyboard :: [Bool] -- List with bools representing if a certain key has been pressed.
               } deriving (Show)

-- Returns a fresh state of the CPU where all of its values has been
-- set to their initial values.
initCPU :: CPU
initCPU = Cpu { v = replicate 16 0
              , i = 0x200
              , sound_timer = 0
              , delay_timer = 0
              , pc = 0x200
              , memory = replicate 4096 0
              , stack = replicate 16 0
              , sp = 0
              , vram = replicate 10 []
              , keyboard = replicate 16 False
              }

-- Fontset for drawing characters to the screen.
fontset :: [Int]
fontset = [ 0xF0, 0x90, 0x90, 0x90, 0xF0 -- 0
          , 0x20, 0x60, 0x20, 0x20, 0x70 -- 1
          , 0xF0, 0x10, 0xF0, 0x80, 0xF0 -- 2
          , 0xF0, 0x10, 0xF0, 0x10, 0xF0 -- 3
          , 0x90, 0x90, 0xF0, 0x10, 0x10 -- 4
          , 0xF0, 0x80, 0xF0, 0x10, 0xF0 -- 5
          , 0xF0, 0x80, 0xF0, 0x90, 0xF0 -- 6
          , 0xF0, 0x10, 0x20, 0x40, 0x40 -- 7
          , 0xF0, 0x90, 0xF0, 0x90, 0xF0 -- 8
          , 0xF0, 0x90, 0xF0, 0x10, 0xF0 -- 9
          , 0xF0, 0x90, 0xF0, 0x90, 0x90 -- A
          , 0xE0, 0x90, 0xE0, 0x90, 0xE0 -- B
          , 0xF0, 0x80, 0x80, 0x80, 0xF0 -- C
          , 0xE0, 0x90, 0x90, 0x90, 0xE0 -- D
          , 0xF0, 0x80, 0xF0, 0x80, 0xF0 -- E
          , 0xF0, 0x80, 0xF0, 0x80, 0x80 -- F
          ]
