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
        setKeyTrue '1' keys = setAt 0 keys True
        setKeyTrue '2' keys = setAt 1 keys True
        setKeyTrue '3' keys = setAt 2 keys True
        setKeyTrue '4' keys = setAt 3 keys True
        setKeyTrue 'q' keys = setAt 4 keys True
        setKeyTrue 'w' keys = setAt 5 keys True
        setKeyTrue 'e' keys = setAt 6 keys True
        setKeyTrue 'r' keys = setAt 7 keys True
        setKeyTrue 'a' keys = setAt 8 keys True
        setKeyTrue 's' keys = setAt 9 keys True
        setKeyTrue 'd' keys = setAt 10 keys True
        setKeyTrue 'f' keys = setAt 11 keys True
        setKeyTrue 'z' keys = setAt 12 keys True
        setKeyTrue 'x' keys = setAt 13 keys True
        setKeyTrue 'c' keys = setAt 14 keys True
        setKeyTrue 'v' keys = setAt 15 keys True
        setKeyTrue _ keys = keys

-- Called every frame before onRenderer
onUpdate :: Float -> CPU -> CPU
onUpdate time cpu = cpu

{-  setAt index list elem
    PRE: index >= 0 && length list > index
-}
setAt :: Int -> [a] -> a -> [a]
setAt 0 (_:xs) z = z : xs
setAt v (x:xs) z = x : (setAt (v-1) xs z)