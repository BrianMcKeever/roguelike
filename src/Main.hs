import qualified Data.Map as Map
import Data.Maybe as Maybe
import Graphics.Gloss.Game hiding (play)
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad.State.Lazy
import Tiles
import EntityComponentSystem
import World

update :: Float -> GameState -> GameState
update _ world = world 

draw :: Map.Map String Picture -> GameState -> Picture
draw tiles _ = pictures [ 
    translate  100  100 (Maybe.fromJust $ Map.lookup "grass" tiles),
    translate  100  100 (Maybe.fromJust $ Map.lookup "tree" tiles)
    ]
    
handleInput :: Event -> GameState -> GameState
handleInput _event                         world            = world

main :: IO ()
main = do
    tiles <- loadTiles
    let ((Entity serial components), state) = runState createEntity initialGameState
    putStr $ show serial
    let (Entity serial components) = evalState createEntity state
    putStr $ show serial
    play (InWindow windowTitle (600, 400) (50, 50)) white 30 initialGameState (draw tiles) handleInput update
    return ()

windowTitle :: String
windowTitle = "Rogue Bard"
