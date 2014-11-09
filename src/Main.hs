import Components.Renderable
import Components.SimpleMovement
import Components.TransformAndPhysics
import Control.Monad
import Control.Monad.State.Lazy
import qualified Data.Map as Map
--import Data.Maybe
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
import World

draw :: GameData -> IO Picture
draw gameData = return $ pictures pictureList
    where
    pictureOnly (RenderData _ p) = p
    pictureList = map pictureOnly $ toBeRendered gameData

handleInput :: Event -> GameData -> IO GameData
handleInput (EventKey (MouseButton RightButton) Up _ (x, y)) gameData = 
    execStateT (setDestination (Location $ H.Vector (float2Double x) $ float2Double y) $ player gameData) gameData
handleInput _ gameData = return gameData

main :: IO ()
main = do
    H.initChipmunk
    gameData <- initialGameData
    tiles' <- loadTiles
    gameData' <- execStateT (createPlayer (H.Vector 0 0)) gameData
    gameData'' <- execStateT loadMap gameData' {tiles = tiles'}
    playIO (InWindow windowTitle (windowWidth, windowHeight) (50, 50)) white 30 gameData'' draw handleInput update

update :: Float -> GameData -> IO GameData
update tick gameData = do
    gameData' <- execStateT f gameData
    H.step (space gameData') $ float2Double tick
    return gameData'
    where
    f :: GameState ()
    f = do
        updateGame tick
        updateGraphics tick

updateEntityGraphic :: Float -> Entity -> GameState ()
updateEntityGraphic tick entity = do
    gameData <- get
    let renderFunctions' = renderFunctions gameData
    let renderFunction = renderFunctions' Map.! entity
    renderFunction tick entity

updateGame :: Float -> GameState ()
updateGame tick = do
    gameData <- get
    updateSimpleMovement tick $ Set.toList $ entities gameData
    --todo we should limit which entities get updated

updateGraphics :: Float -> GameState ()
updateGraphics tick = do
    gameData <- get
    entities' <- liftIO $ filterM (willBeShown gameData) $ Set.toList $ entities gameData
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
        let result = hasRenderable entity gameData'
        return $ result && result2

windowTitle :: String
windowTitle = "Rogue Bard"

windowWidth :: Int
windowWidth = 704

windowHeight :: Int
windowHeight = 704
