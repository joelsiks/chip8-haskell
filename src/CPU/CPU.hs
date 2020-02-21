
data CPU = Cpu { 
    opcode :: (Int, Int, Int, Int), -- Current opcode.
    v :: [Int],                     -- V Register containing 16 8-bit registrars. Index 0, 1, 2 ... E, F.
    i :: Int,                       -- 16 bit register for memory address.
    sound_timer :: Int,             
    delay_timer :: Int,             
    pc :: Int,                      -- Place in memory for current opcode.
    memory :: [Int],                -- Stored program. 4096 bytes.
    stack :: [Int] ,                -- Stack
    sp :: Int,                      -- Place in the stack right now.
    vram :: [[Int]],                -- Memory containing what pixels are to be drawed on the screen.
    keyboard :: [Bool]              -- List with bools representing if a certain key has been pressed.
    } deriving (Show)

-- Returns a newly generated instance of a CPU.
initializeCpu :: CPU
initializeCpu = Cpu {
                  opcode = 0,
                  v = replicate 16 0,
                  i = 0x200,
                  sound_timer = 0,
                  delay_timer = 0,
                  pc = 0x200,
                  memory = replicate 4096 0,
                  stack = replicate 16 0,
                  sp = 0,
                  vram = replicate 10 [],
                  keyboard = replicate 16 False
                  }

