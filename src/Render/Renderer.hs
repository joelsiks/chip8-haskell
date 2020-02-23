module Renderer (DisplaySettings,HandleKeyFunc,UpdateFunc,RendererFunc,GameState,startRenderer) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import Data.Word
import Data.ByteString (ByteString, pack) -- Same as a bitmap

type HandleKeyFunc = Char -> GameState -> GameState
type UpdateFunc = Float -> GameState -> GameState
type RendererFunc = GameState -> [[Bool]]


data DisplaySettings = Settings
    {
        name       :: String,
        size       :: (Int, Int),
        background :: Color,
        fps        :: Int
    } deriving (Show)
st = Settings "Game" (64,32) blue 60

data GameState = State

createFrame :: DisplaySettings -> [[Bool]] -> Picture
createFrame s pixels = bitmapOfByteString sizeX sizeY (BitmapFormat (TopToBottom) (PxRGBA)) bitmapData False
    where
        (sizeX, sizeY) = (size s)
        bitmapData = createBitmapData $ concat pixels

        createBitmapData :: [Bool] -> ByteString
        createBitmapData a = pack $ foldl f [] a
            where
                onCollor  = [255,255,255,255] :: [Word8]
                offCollor = [0,0,0,255]       :: [Word8]
                
                f :: [Word8] -> Bool -> [Word8]
                f a True  = a ++ onCollor
                f a _     = a ++ offCollor

window :: DisplaySettings -> Display
window s = InWindow (name s) (size s) (0,0)

renderer :: DisplaySettings -> RendererFunc -> GameState -> Picture
renderer s f state = createFrame s $ f state

handleKeys :: HandleKeyFunc -> Event -> GameState -> GameState
handleKeys f (EventKey (Char key) Down _ _) game = f key game
handleKeys _ _ game = game

startRenderer :: DisplaySettings -> GameState -> RendererFunc -> HandleKeyFunc -> UpdateFunc -> IO()
startRenderer s gS rF hF uF = play (window s) (background s) (fps s) gS (renderer s rF) (handleKeys hF) uF 

{-
    Examples: for 0 (<= 4) (+1) $ do (putStrLn "1") == "1 \n 1 \n 1 \n 1" 
-}
for :: (Eq a) => a -> (a -> Bool) -> (a -> a) -> IO () -> IO ()
for a eqs eqa io = do
    if (eqs a)
    then do 
        io
        for (eqa a) eqs eqa io
    else do (return ())
