module Main where

import CPU.CPU
import CPU.LoadRom

import System.Random

main :: IO ()
main = do
    let ghciTestPath = "../roms/PONG"
    let cabalRunTestPath = "roms/PONG"
    rom <- CPU.LoadRom.readRom cabalRunTestPath
    let cpu = CPU.CPU.initCPU rom (mkStdGen 0)
    -- bätre slumpgenerering
    -- skicka vidare CPU här
    putStrLn "Hello, Haskell!"