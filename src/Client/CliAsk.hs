
module Client.CliAsk (getRomInfo) where

import Data.Char
import System.Directory
import System.Environment
import System.IO

{-  getRomInfo usingCabal
    asks the user to chose a game found in the specified path and fetches the relative path to that game

    PRE: there are files in "roms" folder
    RETURNS: The relative path to a game
    Examples: getRomInfo False (Input TANK) == "../roms/TANK"
              getRomInfo True (Input TANK)  == "roms/TANK"
-}
getRomInfo :: Bool -> IO (String, Int)
getRomInfo iscabal = do
    let pathStart = if iscabal then "roms/" else "../roms/"
    options <- listDirectory pathStart
    game <- askForFile options
    return (pathStart ++ game, getFPS game)

{-  askForFile path options
    repeatedly asks the user to chose one of the games of the given options and repeats the question if input is invalid.
    returns the chosen games relative path and fps.

    PRE: options is not empty, path exists
    RETURNS: The relative path to a game and game specific fps
    Examples: askForFile [TANK,PONG] (Input TANK) == "TANK"
-}
askForFile :: [String] -> IO String
askForFile options = do
    putStrLn $ "Available games: " ++ buildString options
    putStr "Type in the ROM you would like to launch: "
    hFlush stdout
    inputStr <- getLine
    let str = map toUpper inputStr

    if str `elem` options
    then return str
    else do
        putStrLn "Invalid input. Try again!\n"
        askForFile options

getFPS :: String -> Int
getFPS key = findInList key list
    where
        list = [("15PUZZLE",320),("BLINKY",600),("CONNECT4",50),("HIDDEN", 80),("KALEID",600),("MAZE",300),("PONG",300),("TETRIS",120),("TICTAC",80),("VERS",120)]

        findInList :: String -> [(String, Int)] -> Int
        findInList str ((key,fps):xs)
            | str == key = fps
            | otherwise  = findInList str xs
        findInList _ _   = 100

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