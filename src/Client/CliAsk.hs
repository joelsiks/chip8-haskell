
module Client.CliAsk (getFilePath) where

import System.Directory
import System.Environment

getFilePath :: Bool -> IO String
getFilePath iscabal = do
    let pathStart = if iscabal then "roms/" else "../roms/"
    options <- listDirectory pathStart
    askForFile pathStart options

askForFile :: String -> [String] -> IO String
askForFile pathStart options = do
    putStrLn $ "Available games: " ++ buildString options
    str <- getLine

    if str `elem` options
    then return $ pathStart ++ str
    else do
        putStrLn "Invalid input. Try again!\n"
        askForFile pathStart options

buildString :: [String] -> String
buildString [x]     = x
buildString (x:xs) = x ++ (',':' ':buildString xs)