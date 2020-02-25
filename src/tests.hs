
import Test.HUnit
import CPU.CPU as CPU
import CPU.Emulate as Emulate
import CPU.Utility as Util
import CPU.LoadRom as LoadRom
import System.Random (mkStdGen)

blankCPU = initCPU [0] (mkStdGen 0)

-------------------------------- 
-- CPU Tests

--initCPU
-- Timers should always be >= 0
testICPU1 =
    let cpu = CPU.initCPU [1,2,3] (mkStdGen 0)
    in
        TestCase $ assertBool "initCPU [1,2,3] (mkStdGen 0)" (sound_timer cpu >= 0 && delay_timer cpu >= 0)

-- Memory size is constant
testICPU2 =
    let cpu = CPU.initCPU [1,2,3] (mkStdGen 0)
    in
        TestCase $ assertEqual "initCPU [1,2,3] (mkStdGen 0)" 4096 (length (memory cpu))

-- Vram should be blank and a constant size of 64*32
testICPU3 =
    let cpu = CPU.initCPU [1,2,3] (mkStdGen 0)
    in
        TestCase $ assertEqual "initCPU [1,2,3] (mkStdGen 0)" (replicate (64*32) 0) (concat (vram cpu))


-- initMemory
-- Check that memory is the correct size
testIM1 = TestCase $ assertEqual "initMemory [1,2,3]" 4096 (length (CPU.initMemory [1,2,3]))
-- Check that fontset loaded correctly
testIM2 = TestCase $ assertEqual "initMemory [1]" 0x90 ((CPU.initMemory [1]) !! 2)
-- Check that program loaded at correct index
testIM3 = TestCase $ assertEqual "initMemory [0xA,0xB,0xC]" 0xB ((CPU.initMemory [0xA,0xB,0xC]) !! 513)

-- padRom
-- Program with padding should be constant size 
testPR = TestCase $ assertEqual "padRom [1]" 3584 (length (CPU.padRom [1]))

cpuTests = TestList [testICPU1, testICPU2, testICPU3, testIM1, testIM2, testIM3, testPR]

--------------------------------
-- Rom Tests

-- checkRom
testCR1 = TestCase $ assertEqual "checkRom [1,2,3]" [1,2,3] (LoadRom.checkRom [1,2,3])
testCR2 = TestCase $ assertEqual "checkRom ([4097 zeros])" 0 (length (LoadRom.checkRom (replicate 0 4097)))

romTests = TestList [testCR1, testCR2]

-------------------------------- 
-- Utility tests

-- splitByte
testSB1 = TestCase $ assertEqual "splitByte 0xAF" 
              (0xA, 0xF) (Util.splitByte 0xAF)

testSB2 = TestCase $ assertEqual "splitByte 0x13" 
              (0x1, 0x3) (Util.splitByte 0x13)

testSB3 = TestCase $ assertEqual "splitByte 0x0"  
              (0, 0) (Util.splitByte 0x0)

-- replace
testREP1 = TestCase $ assertEqual "replace 0 0 [1..4]" 
               [0,2,3,4] (Util.replace 0 0 [1..4])

-- nestedListIndexes
testNLI1 = TestCase $ assertEqual "nestedListIndexes 1 1" 
               [(0,0),(0,1),(1,0),(1,1)] (Util.nestedListIndexes 1 1)

utilityTests = TestList [testSB1, testSB2, testSB3, testREP1, testNLI1]

-------------------------------- 
-- Emulate Tests

-- fetchOpcode
-- Fetching a "fake" opcode from the start of the memory where the fontset is located should always yield the same result.
testFOp1 = TestCase $ assertEqual "fetchOpcode" 
               (0xF, 0x0, 0x9, 0x0) (Emulate.fetchOpcode (blankCPU {pc = 0}))

-- opNNN
testOPNNN = TestCase $ assertEqual "opNNN (0xA, 0xB, 0xC, 0xD)"
                0xBCD (Emulate.opNNN (0xA, 0xB, 0xC, 0xD))

-- opNN
testOPNN = TestCase $ assertEqual "opNN (0x1, 0x2, 0x3, 0x4)"
                0x34 (Emulate.opNN (0x1, 0x2, 0x3, 0x4))

-- incPC
-- When increasing the program counter it should always be greater than it was before. Exact implementation may vary.
testIncPC = TestCase $ assertBool "incPC" 
                (pc (Emulate.incPC blankCPU) > pc blankCPU)

-- jumpToAddress - OPCODE: 0x1NNN
testJump = TestCase $ assertEqual "jumpToAddress" 
               0x100 (pc (Emulate.jumpToAddress blankCPU 0x100))

-- insertToStack
testInsertStack = 
    let
      value = 0xFF
      ucpu = Emulate.insertToStack blankCPU value
    in
      TestCase $ assertEqual "insertToStack" 
        value (stack ucpu !! (sp ucpu - 1))

-- returnFromSubroutine
testRetSub =
    let
      callAddr = 0x200
      ucpu = Emulate.returnFromSubroutine (Emulate.callSubroutine blankCPU callAddr)
    in
      TestCase $ assertEqual ("returnFromSubroutine (cpu that has been called to " ++ show callAddr ++ ")")
        (pc (incPC blankCPU)) (pc ucpu)

-- skipInstructionIf
testSkipIf1 = TestCase $ assertEqual "skipInstructionIf blankCPU False"
                  (pc blankCPU) (pc (skipInstructionIf blankCPU False))

testSkipIf2 = TestCase $ assertEqual "skipInstructionIf blankCPU True"
                  (pc (Emulate.incPC blankCPU)) (pc (skipInstructionIf blankCPU True))

-- setRegister
testSetReg =
    let
      index = 0xA
      value = 0xFF
      ucpu = setRegister blankCPU index value
    in
      TestCase $ assertEqual ("setRegister blankCPU "  ++ show index ++ " " ++ show value)
        value (v ucpu !! index)

-- setMemory
testSetMem =
    let
      index = 0xA
      value = 0xFF
      ucpu = setMemory blankCPU index value
    in
      TestCase $ assertEqual ("setMemory blankCPU "  ++ show index ++ " " ++ show value)
        value (memory ucpu !! index)

-- storeBCDRepresentation
testStoreBCD =
  let 
    index = 0x0
    value = 0xFF
    ucpu1 = blankCPU {v = Util.replace index value (v blankCPU)}
    ucpu2 = Emulate.storeBCDRepresentation ucpu1 index
  in
    TestCase $ assertEqual ("storeBCDRepresentation blankCPU " ++ show index)
      value ((memory ucpu2 !! i ucpu2) * 100 + (memory ucpu2 !! (i ucpu2 + 1)) * 10 + memory ucpu2 !! (i ucpu2 + 2))

-- storeRegisters
testStoreReg = 
  let
    amount = 0xF
    ucpu1 = blankCPU {i = 0, v = [0..15]}
    ucpu2 = storeRegisters ucpu1 amount
  in
    TestCase $ assertEqual ("storeRegisters (blankCPU where i=0 & v = [0..15]) " ++ show amount)
      (v ucpu1) (take (amount+1) (memory ucpu2))

-- loadRegisters
testLoadReg = 
  let
    amount = 0xA
    ucpu1 = blankCPU {i = 0}
    ucpu2 = loadRegisters ucpu1 amount
  in
    TestCase $ assertEqual ("loadRegisters (blankCPU where i=0 & v = [0..15]) " ++ show amount)
      (take amount CPU.fontset) (take amount (v ucpu2))
        

emulateTests = TestList [ testOPNNN, testOPNN, testFOp1, testIncPC, testJump, testInsertStack, testRetSub
                        , testSkipIf1, testSkipIf2, testSetReg, testSetMem, testStoreBCD, testStoreReg
                        , testLoadReg
                        ]

-------------------------------- 

runtests = runTestTT $ TestList [cpuTests, romTests, utilityTests, emulateTests]