module Client.CliAsk (getFilePath) where

import System.Directory
import System.Environment

getFilePath :: Bool -> IO String
getFilePath iscabal = do
    let pathStart = if iscabal then "roms/" else "../roms/"
    options <- listDirectory pathStart
    filepath <- askForFile pathStart options
    return filepath

askForFile :: String -> [String] -> IO String
askForFile pathStart options = do
    putStrLn $ "Select a game:  " ++ buildString options
    str <- getLine

    if (elem str options)
    then return $ pathStart ++ str
    else do
        putStrLn "Wrong input!\n"
        askForFile pathStart options

buildString :: [String] -> String
buildString [x]     = x
buildString (x:xs) = x ++ (',':' ':buildString xs)