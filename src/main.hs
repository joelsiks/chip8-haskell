module Main where

import CPU.CPU
import CPU.LoadRom
import Render.Renderer
import Graphics.Gloss(blue)

import System.Random

main :: IO ()
main = do
    let testPath = "../roms/PONG"
    rom <- CPU.LoadRom.readRom testPath
    let cpu = CPU.CPU.initCPU rom (mkStdGen 0)
    -- skicka vidare CPU här

    let displaySettings = Settings "Test" blue 60
    startRenderer displaySettings cpu onRender onInput onUpdate

-- Called last every frame
onRender :: CPU -> [[Bool]]
onRender cpu = undefined

-- Celled on input
onInput :: Char -> CPU -> CPU
onInput key cpu = undefined

-- Called every frame before onRenderer
onUpdate :: Float -> CPU -> CPU
onUpdate time cpu = undefined


