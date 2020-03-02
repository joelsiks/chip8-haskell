
module Render.Renderer (DisplaySettings(..), startRenderer, handleKeys) where

import CPU.CPU (CPU(..), isRunning, startCPU)
import Render.Splash as Splash
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Data.Word
import Data.ByteString (ByteString, pack) -- Same as a bitmap

{- Represents a holder for all display variables.
   Stores variables used for the creaton of a display.

   INVARIANT: used for functions asociated with startRenderer
-}
data DisplaySettings = Settings { size :: (Int, Int)
                                , fps  :: Int
                                } deriving (Show)

{- createFrame settings pixels.
   converts the given pixels into a renderer readable picture.

   PRE: the number of pixels is equal to the number of pixels required for the given screen size
   RETURNS: a renderer readable picture created from the given pixels
   EXAMPLES: createFrame (replicate (64*32) 1) == (An entirely white picture)
-}
createFrame :: [Int] -> Picture
createFrame pixels = bitmapOfByteString 64 32 (BitmapFormat TopToBottom PxRGBA) bitmapData False
  where
    bitmapData = pack $ concatMap f pixels

    f :: Int -> [Word8]
    f 0 = [0,0,0,255]
    f _ = [255,255,255,255]

{- renderer settings cpu
   Creates an image from the vram component of cpu and scales it to fill the screen.

   PRE: cpu is in a functional state
   RETURNS: a renderer readable picture created from the given cpu
   EXAMPLES: renderer (Settings (64,32) 60) (default cpu)                          
             == (A black picture at 1x scale)
             renderer (Settings (64,32) 60) (default cpu where isRunning == False) 
             == (The splash screen picture at 1x scale)
-}
renderer :: DisplaySettings -> CPU -> Picture
renderer s cpu
  | not $ isRunning cpu = scale (x/64) (y/32) $ createFrame Splash.splash
  | otherwise           = scale (x/64) (y/32) $ createFrame $ concat (vram cpu)
    where
      (a, b) = size s
      x      = realToFrac a
      y      = realToFrac b

{- handleKeys func event cpu
   Applies func to cpu whenever a key pressed event is called.

   PRE: cpu is in a functional state
        func is the onInput function found in the Main module
   EXAMPLES: handleKeys (onInput function from main) (EventKey (Char 'q') Down (Modifiers Down Down Down)) (default cpu) 
             == (cpu where keyboard at index 4 is True)
             handleKeys (onInput function from main) (EventKey (Char 'q') Down (Modifiers Down Down Down)) (default cpu where isRunning == False)
             == (default cpu)
             handleKeys (onInput function from main) (EventKey (Char 'k') Down (Modifiers Down Down Down)) (default cpu) 
             == (default cpu)
-}
handleKeys :: (Char -> Bool -> CPU -> CPU) -> Event -> CPU -> CPU
handleKeys f (EventKey a s _ _) cpu
  | not $ isRunning cpu = startCPU cpu
  | otherwise           = handleKeys' f (a,s) cpu
    where
      handleKeys' f (Char key, Down) cpu = f key True  cpu
      handleKeys' f (Char key,   Up) cpu = f key False cpu
      handleKeys' _ _ cpu                  = cpu
handleKeys _ _ cpu = cpu -- Ignores unwated inputs

{- startRenderer settings cpu hFunc uFunc
   Starts a game loop.

   PRE: cpu is in a functional state,
        The number of pixels is equal to the number of pixels required for the given screen size from settings
   SIDE EFFECTS: Creates a window where the screen is drawn
                 Updates the screen every frame
                 Calls renderer every frame
                 Calls uFunc every frame
                 Calls hFunc everytime a key is pressed
                 Excape key stopps the loop
-}
startRenderer :: DisplaySettings -> CPU -> (Char -> Bool -> CPU -> CPU) -> (Float -> CPU -> CPU) -> IO()
startRenderer s cpu hF uF = play FullScreen white (fps s) cpu (renderer s) (handleKeys hF) uF
