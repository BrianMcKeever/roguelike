import Components.Position
import Components.Renderable
import Components.RenderKindFunctions
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Entities.Player
import EntityComponentSystem
import GameState
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Game hiding (play)
import Graphics.Gloss.Interface.Pure.Game
import Tiles
import World
--import Debug.Trace

draw :: GameState -> Picture
draw gameState = pictures pictureList
    where
    pictureOnly (RenderData _ p) = p
    pictureList = map pictureOnly $ toBeRendered gameState

handleInput :: Event -> GameState -> GameState
handleInput _event gameState = gameState

main :: IO ()
main = do
    tiles' <- loadTiles
    let gameState = createPlayer (fromIntegral windowWidth/64/2, fromIntegral windowHeight/64/2) $ loadMap initialGameState {tiles = tiles'}
    play (InWindow windowTitle (windowWidth, windowHeight) (50, 50)) white 10 gameState draw handleInput update

update :: Float -> GameState -> GameState
update tick = updateGraphics tick . updateGame tick 

updateEntityGraphic :: Float -> GameState -> Entity -> GameState
updateEntityGraphic tick gameState entity@(Entity serial kind _) = renderFunction tick gameState' entity
    where
    renderFunctions' = renderFunctions gameState
    maybeRenderFunction = Map.lookup serial renderFunctions'
    (renderFunction, gameState') = if isJust maybeRenderFunction
        then (fromJust maybeRenderFunction, gameState)
        else let renderFunction' = renderKindFunctions Map.! kind in
            (renderFunction', gameState{renderFunctions = Map.insert serial renderFunction renderFunctions'})
            --I'm loading the render function here because doing it in a saner
            --place causes circular imports

updateGame :: Float -> GameState -> GameState
updateGame tick gameState = List.foldl' updateEntity gameState $ Map.keys $ entities gameState 
    where
    ifEntity gameState' serial f = maybe gameState' f $ Map.lookup serial $ entities gameState'

    updateEntity :: GameState -> Serial -> GameState
    updateEntity gameState' serial = ifEntity gameState' serial f
        where
        f (Entity _ _ components) = componentFoldl (updateComponent serial) gameState' components
    -- I'm passing serial numbers instead of entity instances because
    -- passing old copies of possibly updated entities would cause
    -- problems.    

    updateComponent :: Serial -> GameState -> Component -> GameState
    updateComponent serial gameState' component = ifEntity gameState' serial f
        where
        f entity@(Entity _ _ components) = if Set.notMember component components
            then gameState' 
            else (updateFunctions Map.! component) tick gameState' entity
    --Passing old copies of components doesn't matter because they store their
    --state in game state.

updateGraphics :: Float -> GameState -> GameState
-- Get entities that are renderable.
-- Keep the entities that are within the screen area plus half the width of our
-- biggest sprite.
-- Tell these entities to prepare their pictures.
updateGraphics tick gameState = Map.foldl' (updateEntityGraphic tick) gameState {toBeRendered = []} entities'
    where
    box = ((0, 0), (1000, 1000))
    willBeShown entity = hasComponent entity renderableComponent && withinBox gameState box entity
    entities' = Map.filter willBeShown $ entities gameState

windowTitle :: String
windowTitle = "Rogue Bard"

windowWidth :: Int
windowWidth = 704

windowHeight :: Int
windowHeight = 704
