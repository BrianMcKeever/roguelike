import Components.Renderable
import Components.RenderKindFunctions
import Components.SimpleMovement
import Components.Transform
import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Entities.Player
import EntityComponentSystem
import GHC.Float
import GameState
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Game hiding (play)
import Graphics.Gloss.Interface.IO.Game
import qualified Physics.Hipmunk as H
import Tiles
import UpdateFunctions
import World

draw :: GameState -> IO Picture
draw gameState = return $ pictures pictureList
    where
    pictureOnly (RenderData _ p) = p
    pictureList = map pictureOnly $ toBeRendered gameState

handleInput :: Event -> GameState -> IO GameState
handleInput (EventKey (MouseButton RightButton) Up _ (x, y)) gameState = do
    return $ setDestination (Location $ H.Vector (float2Double x) $ float2Double y) gameState $ getPlayer gameState
handleInput _ gameState = return gameState

main :: IO ()
main = do
    H.initChipmunk
    gameState <- initialGameState
    tiles' <- loadTiles
    gameState' <- loadMap gameState {tiles = tiles'}
    gameState'' <- createPlayer (H.Vector 0 0) gameState'
    playIO (InWindow windowTitle (windowWidth, windowHeight) (50, 50)) white 10 gameState'' draw handleInput update

update :: Float -> GameState -> IO GameState
update tick gameState = do
    gameState' <- updateGame tick gameState
    gameState'' <- updateGraphics tick gameState'
    H.step (space gameState'') $ float2Double tick
    return gameState''

updateEntityGraphic :: Float -> GameState -> Entity -> IO GameState
updateEntityGraphic tick gameState entity@(Entity serial kind _) = do
    let renderFunctions' = renderFunctions gameState
    let maybeRenderFunction = Map.lookup serial renderFunctions'
    if isJust maybeRenderFunction
        -- if we have a render function call it. Otherwise, load
        --  a render function of that type and call that.
        then (fromJust maybeRenderFunction) tick gameState entity
        else do
            let renderFunction' = renderKindFunctions Map.! kind
            let renderFunctions'' = Map.insert serial renderFunction' renderFunctions'
            renderFunction' tick gameState{renderFunctions = renderFunctions''} entity
            --I'm loading the render function here because doing it in a saner
            --place causes circular imports

updateGame :: Float -> GameState -> IO GameState
updateGame tick gameState = foldM updateEntity gameState $ Map.keys $ entities gameState 
    where
    updateEntity :: GameState -> Serial -> IO GameState
    updateEntity gameState' serial = do
        let maybeEntity = Map.lookup serial $ entities gameState'
        if isNothing maybeEntity
            then return gameState'
            else do
                let entity = fromJust maybeEntity
                componentFoldM (updateComponent serial) gameState' $ getComponents entity
    -- I'm passing serial numbers instead of entity instances because
    -- passing old copies of possibly updated entities would cause
    -- problems.    

    updateComponent :: Serial -> GameState -> Component -> IO GameState
    updateComponent serial gameState' component = do
        let maybeEntity = Map.lookup serial $ entities gameState'
        if isNothing maybeEntity
            then return gameState'
            else do
                let entity = fromJust maybeEntity
                if Set.notMember component $ getComponents entity
                then return gameState'
                else (updateFunctions Map.! component) tick gameState' entity

    --Passing old copies of components doesn't matter because they store their
    --state in game state.

updateGraphics :: Float -> GameState -> IO GameState
updateGraphics tick gameState = do
    entities' <- filterM willBeShown $ Map.elems $ entities gameState
    foldM (updateEntityGraphic tick) gameState {toBeRendered = []} entities'
    where
    box = ((0, 0), (1000, 1000))
    -- TODO  make sure this box fits
    willBeShown entity = do
        let result = hasComponent entity renderableComponent
        result2 <- withinBox gameState box entity
        return $ result && result2

windowTitle :: String
windowTitle = "Rogue Bard"

windowWidth :: Int
windowWidth = 704

windowHeight :: Int
windowHeight = 704
