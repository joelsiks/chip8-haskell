module Main where

import CPU.CPU
import CPU.LoadRom
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