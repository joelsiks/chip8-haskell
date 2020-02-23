module Render.Renderer (DisplaySettings,HandleKeyFunc,UpdateFunc,RendererFunc,startRenderer) where

import CPU.CPU
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import Data.Word
import Data.ByteString (ByteString, pack) -- Same as a bitmap

{-  Represents the type of a function that handles user inputs for a game.
    When a game is started it requires some function that handles inputs, that function has this type
    INVARIANT: used for functions asociated with startRenderer
-}
type HandleKeyFunc = Char -> CPU -> CPU

{-  Represents the type of function that handles update calls from a game
    When a game is started it requires some function that handles what to do every frame, that function has this type
    INVARIANT: used for functions asociated with startRenderer
-}
type UpdateFunc    = Float -> CPU -> CPU

{-  Represents the type of function that handles conversion from a CPU state to an image
    When a game is started it requires some function that handles how to convert a CPU state into an image, that function has this type
    INVARIANT: used for functions asociated with startRenderer
-}
type RendererFunc  = CPU -> [[Bool]]

{-  Represents a holder for all display variables
    Stores variables used for the creaton of a display
    INVARIANT: used for functions asociated with startRenderer
-}
data DisplaySettings = Settings
    {
        name       :: String,
        size       :: (Int, Int),
        background :: Color,
        fps        :: Int
    } deriving (Show)

{-  createFrame settings pixels
    converts the given pixels into a renderer readable picture
    PRE: The number of pixels is equal to the number of pixels required for the given screen size from settings
    RETURNS: A renderer readable picture created from the given pixels
-}
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

{-  window settings
    Creates a display from settings
-}
window :: DisplaySettings -> Display
window s = InWindow (name s) (size s) (0,0)

{-  renderer settings func cpu
    Applies createFrame to the pixels created from applying func to cpu
    PRE: cpu is in a functional state
-}
renderer :: DisplaySettings -> RendererFunc -> CPU -> Picture
renderer s f state = createFrame s $ f state

{-  handleKeys func event cpu
    Applies func to cpu whenever a key pressed event is called
    PRE: cpu is in a functional state
-}
handleKeys :: HandleKeyFunc -> Event -> CPU -> CPU
handleKeys f (EventKey (Char key) Down _ _) game = f key game
handleKeys _ _ game = game

{-  startRenderer settings cpu rFunc hFunc uFunc
    Starts a game
    PRE: cpu is in a functional state,
         The number of pixels is equal to the number of pixels required for the given screen size from settings
    SIDE EFFECTS: Creates a window where the screen is drawn
                  Updates the screen every frame
                  Calls rFunc every frame
                  Calls uFunc every frame
                  Calls hFunc everytime a key is pressed
-}
startRenderer :: DisplaySettings -> CPU -> RendererFunc -> HandleKeyFunc -> UpdateFunc -> IO()
startRenderer s gS rF hF uF = play (window s) (background s) (fps s) gS (renderer s rF) (handleKeys hF) uF 