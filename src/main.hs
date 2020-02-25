module Main where

import CPU.CPU
import CPU.LoadRom
import qualified CPU.Utility as Util
import Render.Renderer
import Graphics.Gloss(blue)
import System.Random

main :: IO ()
main = do
  let ghciTestPath = "../roms/PONG"
  let cabalRunTestPath = "roms/PONG"
  rom <- CPU.LoadRom.readRom cabalRunTestPath
  let cpu = CPU.CPU.initCPU rom (mkStdGen 0)
  -- bätre slumpgenerering
  -- skicka vidare CPU här
  putStrLn "Hello, World!"
  
  --let displaySettings = Settings "Test" blue 60
  --startRenderer displaySettings cpu onRender onInput onUpdate

-- Called last every frame
onRender :: CPU -> [Int]
onRender cpu = concat (vram cpu)

-- Celled on input
onInput :: Char -> CPU -> CPU
onInput key cpu = cpu {keyboard = setKeyTrue key (keyboard cpu)}
  where
    setKeyTrue :: Char -> [Bool] -> [Bool]
    setKeyTrue '1' keys = Util.replace 0x0 True keys
    setKeyTrue '2' keys = Util.replace 0x1 True keys
    setKeyTrue '3' keys = Util.replace 0x2 True keys
    setKeyTrue '4' keys = Util.replace 0x3 True keys
    setKeyTrue 'q' keys = Util.replace 0x4 True keys
    setKeyTrue 'w' keys = Util.replace 0x5 True keys
    setKeyTrue 'e' keys = Util.replace 0x6 True keys
    setKeyTrue 'r' keys = Util.replace 0x7 True keys
    setKeyTrue 'a' keys = Util.replace 0x8 True keys
    setKeyTrue 's' keys = Util.replace 0x9 True keys
    setKeyTrue 'd' keys = Util.replace 0xA True keys
    setKeyTrue 'f' keys = Util.replace 0xB True keys
    setKeyTrue 'z' keys = Util.replace 0xC True keys
    setKeyTrue 'x' keys = Util.replace 0xD True keys
    setKeyTrue 'c' keys = Util.replace 0xE True keys
    setKeyTrue 'v' keys = Util.replace 0xF True keys
    setKeyTrue _ keys = keys

-- Called every frame before onRenderer
onUpdate :: Float -> CPU -> CPU
onUpdate time cpu = cpu
