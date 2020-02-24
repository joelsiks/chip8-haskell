module Main where

import CPU.CPU
import CPU.LoadRom

import System.Random

main :: IO ()
main = do
    let testPath = "../roms/PONG"
    rom <- CPU.LoadRom.readRom testPath
    let cpu = CPU.CPU.initCPU rom (mkStdGen 0)
    -- skicka vidare CPU här
    putStrLn "Hello, Haskell!"
