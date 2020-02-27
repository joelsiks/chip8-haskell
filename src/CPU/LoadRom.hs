module CPU.LoadRom where 

import System.IO
import qualified Data.ByteString as B

{- readRom path
     Reads a file byte by byte and converts them to integers

     PRE:  path leads to a valid FilePath
     RETURNS: A list of binary integers from the file in path
     SIDE EFFECTS: Reads the file at path, exception thrown if it does not exist
     EXAMPLES: readRom (FilePath with text file containing 3 characters) = [13,10,35]
  -}
readRom :: FilePath -> IO [Int]
readRom path = do
    file <- B.readFile path
    let binaryList = map fromIntegral (B.unpack file)
    return (checkRom binaryList)


{- checkRom rom
     Ensures that rom is a valid rom

     RETURNS: Empty list if rom is not valid, otherwise rom
     EXAMPLES: checkRom (rom larger than 3584) = []
               checkRom (valid small rom) = rom
  -}
-- TODO: More checks to ensure rom is a valid program
checkRom :: [Int] -> [Int]
checkRom rom 
    | length rom > 0xE00 = []
    -- | more checks
    | otherwise = rom 