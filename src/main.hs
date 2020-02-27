module Main where

import CPU.CPU as CPU
import CPU.LoadRom as LoadRom
import Render.Renderer as Render
import Client.CliAsk as Client
import CPU.Emulate as Emulate (emulateCycle)
import CPU.Utility as Util (replace)
import Graphics.Gloss (blue)
import Graphics.Gloss.Interface.Environment (getScreenSize)
import System.Random
import Data.Time.Clock.POSIX

main :: IO ()
main = do
  path    <- getFilePath True     -- False -> GHCi, True -> Cabal
  rom     <- LoadRom.readRom path
  size    <- getScreenSize
  rndSeed <- fmap round getPOSIXTime
  let displaySettings  = Settings size 300
  let cpu = CPU.initCPU rom (mkStdGen rndSeed)
  startRenderer displaySettings cpu onRender onInput onUpdate

-- Called last every frame
onRender :: CPU -> [Int]
onRender cpu = concat (vram cpu)

-- Called on input
onInput :: Char -> Bool -> CPU -> CPU
onInput key isDown cpu = cpu {keyboard = setKey key isDown (keyboard cpu)}
  where
    setKey :: Char -> Bool -> [Bool] -> [Bool]
    setKey '1' b keys = Util.replace 0x1 b keys
    setKey '2' b keys = Util.replace 0x2 b keys
    setKey '3' b keys = Util.replace 0x3 b keys
    setKey '4' b keys = Util.replace 0xC b keys
    setKey 'q' b keys = Util.replace 0x4 b keys
    setKey 'w' b keys = Util.replace 0x5 b keys
    setKey 'e' b keys = Util.replace 0x6 b keys
    setKey 'r' b keys = Util.replace 0xD b keys
    setKey 'a' b keys = Util.replace 0x7 b keys
    setKey 's' b keys = Util.replace 0x8 b keys
    setKey 'd' b keys = Util.replace 0x9 b keys
    setKey 'f' b keys = Util.replace 0xE b keys
    setKey 'z' b keys = Util.replace 0xA b keys
    setKey 'x' b keys = Util.replace 0x0 b keys
    setKey 'c' b keys = Util.replace 0xB b keys
    setKey 'v' b keys = Util.replace 0xF b keys
    setKey _ _ keys = keys

-- Called every frame before onRenderer
onUpdate :: Float -> CPU -> CPU
onUpdate _ cpu = Emulate.emulateCycle cpu
