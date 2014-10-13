import qualified Data.Map as Map
import Data.Maybe as Maybe
import Graphics.Gloss.Game
import Graphics.Gloss.Interface.IO.Game
import Tiles
import EntityComponentSystem
import World

applyVelocity :: a -> World -> IO World
applyVelocity _ world
  = do
  return world 

draw :: Map.Map String Picture -> World -> IO Picture
draw tiles _ = return $ pictures [ 
    translate  100  100 (Maybe.fromJust $ Map.lookup "grass" tiles),
    translate  100  100 (Maybe.fromJust $ Map.lookup "tree" tiles)
    ]
    
handleInput :: Event -> World -> IO World
handleInput (EventKey (Char 'a') Down _ _)            world = return world 
handleInput (EventKey (Char 'd') Down _ _)            world = return world
handleInput (EventKey (SpecialKey KeySpace) Down _ _) world = return world
handleInput _event                         world            = return world
    
main :: IO ()
main = do
    tiles <- loadTiles
    initialWorld <-  createWorld
    playIO (InWindow windowTitle (600, 400) (50, 50)) white 30 initialWorld (draw tiles) handleInput applyVelocity

windowTitle :: String
windowTitle = "Rogue Bard"
