
module Client.CliAsk (getFilePath) where

import System.Directory
import System.Environment

{-  getFilePath usingCabal
    asks the user to chose a game found in the specified path and fetches the relative path to that game

    PRE: there are files in "roms" folder
    RETURNS: The relative path to a game
    Examples: getFilePath False (Input TANK) == "../roms/TANK"
              getFilePath True (Input TANK)  == "roms/TANK"
-}
getFilePath :: Bool -> IO String
getFilePath iscabal = do
    let pathStart = if iscabal then "roms/" else "../roms/"
    options <- listDirectory pathStart
    askForFile pathStart options

{-  askForFile path options
    repeatedly asks the user to chose one of the games of the given options and repeats the question if input is invalid.
    returns the chosen games relative path.

    PRE: options is not empty, path exists
    RETURNS: The relative path to a game
    Examples: getFilePath "../roms/" (Input TANK) == "../roms/TANK"
-}
askForFile :: String -> [String] -> IO String
askForFile pathStart options = do
    putStrLn $ "Available games: " ++ buildString options
    str <- getLine

    if str `elem` options
    then return $ pathStart ++ str
    else do
        putStrLn "Invalid input. Try again!\n"
        askForFile pathStart options


{-  buildString list
    formats a list into a nice looking text

    PRE: list is not empty
    RETURNS: list formated into a nice looking text
    EXAMPLES: buildString ["hej"]      == "hej"
              buildString ["hej", "a"] == "hej, a"
-}
buildString :: [String] -> String
buildString [x]    = x
buildString (x:xs) = x ++ (',' : ' ' : buildString xs)