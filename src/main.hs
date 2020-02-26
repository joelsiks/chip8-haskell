module Main where

import CPU.CPU as CPU
import CPU.LoadRom as LoadRom
import CPU.Emulate as Emulate
import qualified CPU.Utility as Util
import Render.Renderer
import Graphics.Gloss (blue)
import Graphics.Gloss.Interface.Environment (getScreenSize)
import System.Random
import Debug.Trace

main :: IO ()
main = do
  let ghciTestPath = "../roms/TEST_OPCODES"
  let cabalRunTestPath = "roms/TEST_OPCODES"
  rom <- LoadRom.readRom cabalRunTestPath
  let cpu = CPU.initCPU rom (mkStdGen 0)
    -- bätre slumpgenerering
  --putStrLn "Hello, World!"
  size <- getScreenSize 
  let displaySettings = Settings size "Test" blue 400
  startRenderer displaySettings cpu onRender onInput onUpdate

-- Called last every frame
onRender :: CPU -> [Int]
onRender cpu = concat (vram cpu)

-- Celled on input
onInput :: Char -> Bool -> CPU -> CPU
onInput key isDown cpu = trace ("Keys: " ++ show key) $ cpu {keyboard = setKey key isDown (keyboard cpu)}
  where
    setKey :: Char -> Bool -> [Bool] -> [Bool]
    setKey '1' b keys = Util.replace 0x0 b keys
    setKey '2' b keys = Util.replace 0x1 b keys
    setKey '3' b keys = Util.replace 0x2 b keys
    setKey '4' b keys = Util.replace 0x3 b keys
    setKey 'q' b keys = Util.replace 0x4 b keys
    setKey 'w' b keys = Util.replace 0x5 b keys
    setKey 'e' b keys = Util.replace 0x6 b keys
    setKey 'r' b keys = Util.replace 0x7 b keys
    setKey 'a' b keys = Util.replace 0x8 b keys
    setKey 's' b keys = Util.replace 0x9 b keys
    setKey 'd' b keys = Util.replace 0xA b keys
    setKey 'f' b keys = Util.replace 0xB b keys
    setKey 'z' b keys = Util.replace 0xC b keys
    setKey 'x' b keys = Util.replace 0xD b keys
    setKey 'c' b keys = Util.replace 0xE b keys
    setKey 'v' b keys = Util.replace 0xF b keys
    setKey _ _ keys = keys

-- Called every frame before onRenderer
onUpdate :: Float -> CPU -> CPU
onUpdate _ cpu = Emulate.emulateCycle cpu
