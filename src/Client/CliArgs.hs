module Client.CliArg where

import System.Directory
import System.Environment

main = do
    let pathStart = if False then "roms/" else "../roms/"
    options <- listDirectory pathStart
    str <- getArgs >>= (parse pathStart options)
    if (elem str options )
    then return $ pathStart ++ str
    else return $ pathStart ++ "PONG"
    
parse _ _ ["-h"] = do 
    putStrLn "Usage: tac [-vh] [file ..]" 
    return ""
parse _ _ ["-f", x] = return x
parse f o ["-l"] = do
    putStrLn $ "Files:  " ++ buildString o
    return ""
parse _ _ _  = return ""

buildString :: [String] -> String
buildString [x]    = x
buildString (x:xs) = x ++ (',':' ':buildString xs)