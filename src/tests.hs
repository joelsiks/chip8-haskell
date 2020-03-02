
import Test.HUnit
import CPU.CPU as CPU
import CPU.Emulate as Emulate
import CPU.Utility as Util
import CPU.LoadRom as LoadRom
import Render.Renderer as Renderer
import CLI.CliAsk as CliAsk
import Data.Bits ((.&.), shiftL, shiftR)
import System.Random (mkStdGen, randomR)
import Graphics.Gloss.Interface.IO.Game (Key(Char), KeyState(..), Event(..), Modifiers(..))

standardGen = mkStdGen 0
blankCPU = (initCPU [0] standardGen) {isRunning = True}

-------------------------------- 
-- CPU Tests

--initCPU
-- Timers should always be >= 0
testICPU1 =
  let cpu = CPU.initCPU [1,2,3] (mkStdGen 0)
  in TestCase $ assertBool "initCPU [1,2,3] (mkStdGen 0)" (sound_timer cpu >= 0 && delay_timer cpu >= 0)

-- Memory size is constant
testICPU2 =
  let cpu = CPU.initCPU [1,2,3] (mkStdGen 0)
  in TestCase $ assertEqual "initCPU [1,2,3] (mkStdGen 0)" 4096 (length (memory cpu))

-- Vram should be blank and a constant size of 64*32
testICPU3 =
  let cpu = CPU.initCPU [1,2,3] (mkStdGen 0)
  in TestCase $ assertEqual "initCPU [1,2,3] (mkStdGen 0)" (replicate (64*32) 0) (concat (vram cpu))

-- Check that running flag is set correctly
testSCPU =
  let cpu = CPU.initCPU [1,2,3] (mkStdGen 0)
  in TestCase $ assertEqual "startCPU cpu" True (isRunning (CPU.startCPU cpu))

-- initMemory
-- Check that memory is the correct size
testIM1 = TestCase $ assertEqual "initMemory [1,2,3]" 4096 (length (CPU.initMemory [1,2,3]))
-- Check that fontset loaded correctly
testIM2 = TestCase $ assertEqual "initMemory [1]" 0x90 (CPU.initMemory [1] !! 2)
-- Check that program loaded at correct index
testIM3 = TestCase $ assertEqual "initMemory [0xA,0xB,0xC]" 0xB (CPU.initMemory [0xA,0xB,0xC] !! 513)

-- padRom
-- Program with padding should be constant size 
testPR = TestCase $ assertEqual "padRom [1]" 3584 (length (CPU.padRom [1]))

cpuTests = TestList [testICPU1, testICPU2, testICPU3, testSCPU, testIM1, testIM2, testIM3, testPR]

--------------------------------
-- Rom Tests

-- checkRom
testCR1 = TestCase $ assertEqual "checkRom [1,2,3]" [1,2,3] (LoadRom.checkRom [1,2,3])
testCR2 = TestCase $ assertEqual "checkRom ([4097 zeros])" 0 (length (LoadRom.checkRom (replicate 0 4097)))

romTests = TestList [testCR1, testCR2]

-------------------------------- 
-- Utility tests

-- splitByte
testSB1 = TestCase $ assertEqual "splitByte 0xAF" (0xA, 0xF) (Util.splitByte 0xAF)

testSB2 = TestCase $ assertEqual "splitByte 0x13" (0x1, 0x3) (Util.splitByte 0x13)

testSB3 = TestCase $ assertEqual "splitByte 0x0"  (0, 0) (Util.splitByte 0x0)

-- replace
testREP1 = TestCase $ assertEqual "replace 0 0 [1..4]" [0,2,3,4] (Util.replace 0 0 [1..4])

-- nestedListIndexes
testNLI1 = TestCase $ assertEqual "nestedListIndexes 1 1" 
               [(0,0),(0,1),(1,0),(1,1)] (Util.nestedListIndexes 1 1)

utilityTests = TestList [testSB1, testSB2, testSB3, testREP1, testNLI1]

-------------------------------- 
-- Emulate Tests

-- Tests that value is stored and timer is decresed
testCycle = 
  let cpu = Emulate.emulateCycle (CPU.initCPU [0x6A,0x02] (mkStdGen 0)) {delay_timer = 9}
  in TestCase $ assertBool "initCPU [0x6A,0x02] (mkStdGen 0)" (v cpu !! 0xA == 2 && delay_timer cpu == 8)

-- Sound timer should be decresed but not delay_timer
testTimers =
  let cpu = decreseTimers $ CPU.initCPU [1,2,3] (mkStdGen 0)
      cpu2 = decreseTimers $ cpu {sound_timer = 7, delay_timer = 0}
  in TestCase $ assertBool "decreseTimers cpu" (sound_timer cpu2 == 6 && delay_timer cpu2 == 0)

-- fetchOpcode
-- Fetching a "fake" opcode from the start of the memory where the fontset is located should always yield the same result.
testFOp1 = TestCase $ assertEqual "fetchOpcode" (0xF, 0x0, 0x9, 0x0) 
               (Emulate.fetchOpcode (blankCPU {pc = 0}))

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
testJump = TestCase $ assertEqual "jumpToAddress 0x100" 
               0x100 (pc (Emulate.jumpToAddress blankCPU 0x100))

-- insertToStack
testInsertStack = 
    let
      value = 0xFF
      ucpu = Emulate.insertToStack blankCPU value
    in
      TestCase $ assertEqual ("insertToStack blankCPU " ++ show value)
        value (stack ucpu !! (sp ucpu - 1))

-- returnFromSubroutine
-- When returning from a subroutine the program counter should be the same as it was but incremented by one instruction.
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
-- Checks if the correct value has been inserted at the correct index.
testSetReg =
  let
    index = 0xA
    value = 0xFF
    ucpu = setRegister blankCPU index value
  in
    TestCase $ assertEqual ("setRegister blankCPU "  ++ show index ++ " " ++ show value)
      value (v ucpu !! index)

-- setMemory
-- Checks if the correct value has been inserted at the correct index.
testSetMem =
  let
    index = 0xA
    value = 0xFF
    ucpu = setMemory blankCPU index value
  in
    TestCase $ assertEqual ("setMemory blankCPU "  ++ show index ++ " " ++ show value)
      value (memory ucpu !! index)

-- storeBCDRepresentation
-- When constructing the value from the memory it should be the same as it was when passed.
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
-- The correct amount of registers should be copied to the memory and with correct values.
testStoreReg = 
  let
    amount = 0xF
    ucpu1 = blankCPU {i = 0, v = [0..15]}
    ucpu2 = storeRegisters ucpu1 amount
  in
    TestCase $ assertEqual ("storeRegisters (blankCPU where i=0 & v = [0..15]) " ++ show amount)
      (v ucpu1) (take (amount+1) (memory ucpu2))

-- loadRegisters
-- The correct amount of registers should be copied from the memory and with correct values.
testLoadReg = 
  let
    amount = 0xA
    ucpu1 = blankCPU {i = 0}
    ucpu2 = loadRegisters ucpu1 amount
  in
    TestCase $ assertEqual ("loadRegisters (blankCPU where i=0 & v = [0..15]) " ++ show amount)
      (take amount CPU.fontset) (take amount (v ucpu2))

-- generateRandomValue
-- blankCPU uses the same standardGen as in this test so they should yield the same result when generating
-- a random number.
testRandVal =
  let
    (randVal, _) = randomR (0, 255) standardGen
    andVal  = 0x3A
    index = 0x0
    ucpu = Emulate.generateRandomValue blankCPU index andVal
  in
    TestCase $ assertEqual ("generateRandomValue blankCPU " ++ show index ++ " " ++ show andVal)
      (v ucpu !! index) (randVal .&. andVal)

-- checkIfInput
testCheckInput1 =
  let
    keyIndex = 0x3
    regIndex = 0x6
    ucpu = blankCPU {keyboard = Util.replace keyIndex True (replicate 16 False)}
  in
    TestCase $ assertEqual ("checkIfInput (cpu where keyboard index " ++ show keyIndex ++ " set to True) " ++ show regIndex)
      (v (checkIfInput ucpu regIndex) !! regIndex) keyIndex

testCheckInput2 = TestCase $ assertEqual "checkIfInput (cpu where every key set to False) 0x0"
                      (keyboard (checkIfInput blankCPU 0x0)) (replicate 16 False)

-- shiftRegLeft
testShiftLeft =
  let
    regIndex = 0x4
    n1 = 224
    n2 = 20
    ucpu1 = setRegister blankCPU regIndex n1
    ucpu2 = setRegister blankCPU regIndex n2
  in
    TestList [
      -- Tests if 8-bit overflow "wraps" around correctly. 
      TestCase $ assertEqual ("shiftRegLeft (cpu where (v cpu !! " ++ show regIndex ++ ") = " ++ show n1 ++ ") " ++ show regIndex)
        (v (shiftRegLeft ucpu1 regIndex) !! regIndex) 192
      -- Tests normal use case.
    , TestCase $ assertEqual ("shiftRegLeft (cpu where (v cpu !! " ++ show regIndex ++ ") = " ++ show n2 ++ ") " ++ show regIndex)
        (v (shiftRegLeft ucpu2 regIndex) !! regIndex) 40
    ]

-- shiftRegRight
testShiftRight =
  let
    regIndex = 0x4
    n1 = 224
    n2 = 20
    ucpu1 = setRegister blankCPU regIndex n1
    ucpu2 = setRegister blankCPU regIndex n2
  in
    TestList [
      TestCase $ assertEqual ("shiftRegRight (cpu where (v cpu !! " ++ show regIndex ++ ") = " ++ show n1 ++ ") " ++ show regIndex)
        (v (shiftRegRight ucpu1 regIndex) !! regIndex) 112
    , TestCase $ assertEqual ("shiftRegRight (cpu where (v cpu !! " ++ show regIndex ++ ") = " ++ show n2 ++ ") " ++ show regIndex)
        (v (shiftRegRight ucpu2 regIndex) !! regIndex) 10
    ]

-------------------------------- 
-- CliAsk Tests

-- getFPS
testGetFPS = TestList [TestCase $ assertEqual "getFPS for PONG" (getFPS "PONG") 300,
                       TestCase $ assertEqual "getFPS for BLINKY" (getFPS "BLINKY") 600,
                       TestCase $ assertEqual "getFPS for undefined rom" (getFPS "X") 100]

-- buildString
testBuildString = TestCase $ assertEqual "buildString for [jag,hello,str]" (buildString ["jag","hello","str"]) "jag, hello, str"

-------------------------------- 
emulateTests = TestList [ testCycle, testTimers, testOPNNN, testOPNN, testFOp1, testIncPC, testJump, testInsertStack, testRetSub
                        , testSkipIf1, testSkipIf2, testSetReg, testSetMem, testStoreBCD, testStoreReg
                        , testLoadReg, testRandVal, testCheckInput1, testCheckInput2, testShiftLeft
                        , testShiftRight, testHandleKeys, testGetFPS, testBuildString
                        ]

-------------------------------- 

runtests = runTestTT $ TestList [cpuTests, romTests, utilityTests, emulateTests]