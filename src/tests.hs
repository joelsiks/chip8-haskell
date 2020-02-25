
import Test.HUnit
import CPU.CPU as CPU
import CPU.Utility as Util
import CPU.LoadRom as LoadRom

-- CPU Tests

-- Utility tests

testSB1 = TestCase $ assertEqual "splitByte 0xAF" (0xA, 0xF) (Util.splitByte 0xAF)
testSB2 = TestCase $ assertEqual "splitByte 0x13" (0x1, 0x3) (Util.splitByte 0x13)
testSB3 = TestCase $ assertEqual "splitByte 0x0"  (0, 0)     (Util.splitByte 0x0)

testREP1 = TestCase $ assertEqual "replace 0 0 [1..4]" [0,2,3,4] (Util.replace 0 0 [1..4])

testNLI1 = TestCase $ assertEqual "nestedListIndexes 1 1" [(0,0),(0,1),(1,0),(1,1)] (Util.nestedListIndexes 1 1)

utilityTests = TestList [testSB1, testSB2, testSB3, testREP1, testNLI1]

-- Emulate Tests