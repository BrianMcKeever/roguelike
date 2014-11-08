import Components.Renderable
import Components.SimpleMovement
import Components.Transform
import Control.Monad
import Control.Monad.State.Lazy
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

draw :: GameData -> IO Picture
draw gameData = return $ pictures pictureList
    where
    pictureOnly (RenderData _ p) = p
    pictureList = map pictureOnly $ toBeRendered gameData

handleInput :: Event -> GameData -> IO GameData
handleInput (EventKey (MouseButton RightButton) Up _ (x, y)) gameData = 
    execStateT (setDestination (Location $ H.Vector (float2Double x) $ float2Double y) $ getPlayer gameData) gameData
handleInput _ gameData = return gameData

main :: IO ()
main = do
    H.initChipmunk
    gameData <- initialGameData
    tiles' <- loadTiles
    gameData' <- execStateT loadMap gameData {tiles = tiles'}
    gameData'' <- execStateT (createPlayer (H.Vector 0 0)) gameData'
    playIO (InWindow windowTitle (windowWidth, windowHeight) (50, 50)) white 10 gameData'' draw handleInput update

update :: Float -> GameData -> IO GameData
update tick gameData = do
    gameData' <- execStateT (updateGame tick) gameData
    gameData'' <- execStateT (updateGraphics tick) gameData'
    H.step (space gameData'') $ float2Double tick
    return gameData''

updateEntityGraphic :: Float -> Entity -> GameState ()
updateEntityGraphic tick entity@(Entity serial _ _) = do
    gameData <- get
    let renderFunctions' = renderFunctions gameData
    let renderFunction = renderFunctions' Map.! serial
    renderFunction tick entity

updateGame :: Float -> GameState ()
updateGame tick = do
    gameData <- get
    foldState updateEntity $ Map.keys $ entities gameData 
    where
    updateEntity :: Serial -> GameState ()
    updateEntity serial = do
        gameData' <- get
        let maybeEntity = Map.lookup serial $ entities gameData'
        unless (isNothing maybeEntity) $ do
            let entity = fromJust maybeEntity
            liftIO $ componentFoldM (updateComponent serial) gameData' $ getComponents entity
            return ()
    -- I'm passing serial numbers instead of entity instances because
    -- passing old copies of possibly updated entities would cause
    -- problems.    

    updateComponent :: Serial -> GameData -> Component -> IO GameData
    updateComponent serial gameData component = do
        let maybeEntity = Map.lookup serial $ entities gameData
        if isNothing maybeEntity
            then return gameData
            else do
                let entity = fromJust maybeEntity
                if Set.notMember component $ getComponents entity
                then return gameData
                else execStateT ((updateFunctions Map.! component) tick entity) gameData

    --Passing old copies of components doesn't matter because they store their
    --state in game state.

updateGraphics :: Float -> GameState ()
updateGraphics tick = do
    gameData <- get
    entities' <- liftIO $ filterM (willBeShown gameData) $ Map.elems $ entities gameData
    put gameData{toBeRendered = []}
    foldState (updateEntityGraphic tick) entities'
    where
    box = ((0, 0), (1000, 1000))
    -- TODO  make sure this box fits
    willBeShown gameData' entity = do
        result2 <- evalStateT (withinBox box entity) gameData'
        --we can use eval because we're not doing anything that changes state.
        --We should probably look at whether getters should be passed game state
        --to begin with
        return $ result && result2
        where
        result = hasComponent entity renderableComponent

windowTitle :: String
windowTitle = "Rogue Bard"

windowWidth :: Int
windowWidth = 704

windowHeight :: Int
windowHeight = 704
