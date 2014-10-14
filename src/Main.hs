import qualified Data.Map as Map
import Data.Maybe as Maybe
import Graphics.Gloss.Game hiding (play)
import Graphics.Gloss.Interface.Pure.Game
import Tiles
import World

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
    let gameState = loadMap initialGameState tiles
    play (InWindow windowTitle (600, 400) (50, 50)) white 30 gameState (draw tiles) handleInput update

update :: Float -> GameState -> GameState
update _ world = world 

windowTitle :: String
windowTitle = "Rogue Bard"
