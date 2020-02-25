
import Test.HUnit
import CPU.CPU as CPU
import CPU.Emulate as Emulate
import CPU.Utility as Util
import CPU.LoadRom as LoadRom
import System.Random (mkStdGen)

blankCPU = initCPU [0] (mkStdGen 0)

-------------------------------- 
-- CPU Tests

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

runtests = runTestTT $ TestList [utilityTests, emulateTests]