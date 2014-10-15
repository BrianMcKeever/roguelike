import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe
import EntityComponentSystem
import Graphics.Gloss.Game hiding (play)
import Graphics.Gloss.Interface.Pure.Game
import Tiles
import World

draw :: Map.Map String Picture -> GameState -> Picture
draw tiles _ = pictures [ 
    translate  100  100 (tiles Map.! "grass"),
    translate  100  100 (tiles Map.! "tree")
    ]
    
handleInput :: Event -> GameState -> GameState
handleInput _event gameState = gameState

main :: IO ()
main = do
    tiles <- loadTiles
    let gameState = loadMap initialGameState
    play (InWindow windowTitle (600, 400) (50, 50)) white 30 gameState (draw tiles) handleInput update

update :: Float -> GameState -> GameState
update tick = updateGraphics tick . updateGame tick 

updateGame :: Float -> GameState -> GameState
updateGame _ gameState = List.foldl' updateEntity gameState $ Map.keys $ entities gameState 
    where
    ifEntity gameState serial f = maybe gameState f $ Map.lookup serial $ entities gameState

    updateEntity :: GameState -> Serial -> GameState
    updateEntity gameState serial = ifEntity gameState serial f
        where
        f (Entity serial components) = componentFoldl (updateComponent serial) gameState components
    -- I'm passing serial numbers instead of entity instances because
    -- passing old copies of possibly updated entities would cause
    -- problems.    

    updateComponent :: Serial -> GameState -> Component -> GameState
    updateComponent serial gameState component = ifEntity gameState serial f
        where
        f entity = (updateFunctions Map.! component) gameState entity
    --Passing old copies of components doesn't matter because they store their
    --state in game state.

updateGraphics :: Float -> GameState -> GameState
updateGraphics _ gameState = gameState 

windowTitle :: String
windowTitle = "Rogue Bard"
