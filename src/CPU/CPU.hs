
module CPU.CPU where

import System.IO
import CPU.LoadRom
import System.Random

windowHeight = 32
windowWidth  = 64

{- Represents a centralised state for the CPU of the CHIP-8.
   The CHIP-8's state is the inner workings of the computer. It holds all the necessary data
   that it needs to have in order to execute any instructions and to interpret any data.

   INVARIANT: TODO
              sound_timer >= 0
              delay_timer >= 0
-}
data CPU = Cpu { v :: [Int]             -- 16 V Registers with 8-bit registrars. Index 0, 1, 2 ... E, F.
               , i :: Int               -- 16 bit register for memory address.
               , sound_timer :: Int     
               , delay_timer :: Int     
               , pc :: Int              -- Pointer to memory for current opcode. 0 =< pc < length memory.
               , memory :: [Int]        -- Place to store program data (instructions). 4096 bytes.
               , stack :: [Int]         -- Stack. List of 16 16-bit values.
               , sp :: Int              -- Pointer to current place in the stack.
               , vram :: [[Int]]        -- Memory containing what pixels are to be drawed on the screen.
               , keyboard :: [Bool]     -- List with bools representing if a certain key has been pressed.
               , keypad_waiting :: Bool -- List with bools representing if a certain key has been pressed.
               , rgen :: StdGen         -- Random number generator.
               } deriving (Show)

-- Returns a fresh state of the CPU where all of its values has been
-- set to their initial values.
initCPU :: [Int] -> StdGen -> CPU
initCPU rom randomgen = Cpu { v = replicate 16 0
                        , i = 0x200
                        , sound_timer = 0
                        , delay_timer = 0
                        , pc = 0x200
                        , memory = initMemory rom
                        , stack = replicate 16 0
                        , sp = 0
                        , vram = defaultVRAM
                        , keyboard = replicate 16 False
                        , keypad_waiting = False
                        , rgen = randomgen
                        }

defaultVRAM :: [[Int]]
defaultVRAM = replicate windowHeight (replicate windowWidth 0)

{- initMemory rom
     Loads the fontset and a program onto the processors memory

     RETURNS: fontset ++ (zeros up to adress 0x200) ++ program ++ (zeros to fill out rest of memory)
     EXAMPLES: initMemory ('rom' containing 3 characters) = fontset ++ (zeros to index 512) ++ [13,10,35] ++ (zeros to index 4096)
  -}
initMemory :: [Int] -> [Int]
initMemory rom = fontset ++ (replicate (0x200 - length fontset) 0) ++ padRom rom

{- padRom rom
     Pads rom with empty data to fill up memory

     RETURNS: a list of length 3854 consisting of rom ++ (replicate (3854 - length rom) 0)
     EXAMPLES: readRom [1,2,3,4] = [1,2,3,4] ++ (replicate 3850 0)
  -}
padRom :: [Int] -> [Int]
padRom rom
  | memLeft < 0 = error "Program too large"
  | rom == [] = error "File error"
  | otherwise = rom ++ (replicate memLeft 0)
      where memLeft = 0xE00 - length rom

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
