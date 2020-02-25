
import Test.HUnit
import CPU.CPU as CPU
import CPU.Emulate as Emulate
import CPU.Utility as Util
import CPU.LoadRom as LoadRom
import System.Random (mkStdGen)

blankCPU = initCPU [0] (mkStdGen 0)

-- CPU Tests

-- Utility tests

testSB1 = TestCase $ assertEqual "splitByte 0xAF" (0xA, 0xF) (Util.splitByte 0xAF)
testSB2 = TestCase $ assertEqual "splitByte 0x13" (0x1, 0x3) (Util.splitByte 0x13)
testSB3 = TestCase $ assertEqual "splitByte 0x0"  (0, 0)     (Util.splitByte 0x0)

testREP1 = TestCase $ assertEqual "replace 0 0 [1..4]" [0,2,3,4] (Util.replace 0 0 [1..4])

testNLI1 = TestCase $ assertEqual "nestedListIndexes 1 1" [(0,0),(0,1),(1,0),(1,1)] (Util.nestedListIndexes 1 1)

utilityTests = TestList [testSB1, testSB2, testSB3, testREP1, testNLI1]

-- Emulate Tests

testFOp1 = TestCase $ assertEqual "fetchOpcode" (0xF, 0x0, 0x9, 0x0) (Emulate.fetchOpcode (blankCPU {pc = 0}))
testIncPC = TestCase $ assertBool "incPC" (pc (Emulate.incPC blankCPU) > pc blankCPU)

-- OPCODE: 0x1NNN
testJump = TestCase $ assertEqual "jumpToAddress" (0x100) (pc (Emulate.jumpToAddress blankCPU 0x100))

emulateTests = TestList [testFOp1, testIncPC, testJump]

---

runtests = runTestTT $ TestList [utilityTests, emulateTests]