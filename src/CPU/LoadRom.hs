module CPU.LoadRom(readRom) where 

import System.IO
import qualified Data.ByteString as B
import Data.Word

path = "../../roms/PONG"
readRom :: FilePath -> IO [Int]
readRom a = do
    file <- B.readFile a
    let list = map fromIntegral (B.unpack file)
    return (genMem list)

genMem :: [Int] -> [Int]
genMem romList
    | memLeft < 0 = error "Program too large"
    | otherwise = (replicate 512 0) ++ romList ++ (replicate memLeft 0)
        where memLeft = 3584 - length romList
