module GameState (
    addComponent,
    createEntity,
    foldState,
    GameData(..),
    GameState,
    getEntity,
    initialGameData,
    removeComponent
)
where
import Components.PhysicsBase
import Components.RenderableBase
import Components.SimpleMovementBase
import Components.TransformBase
import Control.Monad.State.Lazy
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import EntityComponentSystem
import Graphics.Gloss.Game
import qualified Physics.Hipmunk as H
import System.Random

--Only gameState, gameData, and initialCameData deserve to be here. Everything else is here because they were causing circular imports in
--entitycomponentSystem

addComponent :: Component -> Entity -> GameState Entity
addComponent component (Entity serial kind components) = do
    let entity = Entity serial kind $ Set.insert component components
    gameData <- get
    let entities' = Map.insert serial entity $ entities gameData
    put gameData{entities = entities'}
    return entity
  
createEntity :: Kind -> GameState Entity
createEntity kind = do
    serial <- getNextSerial
    let entity = Entity serial kind emptyComponents
    gameData <- get
    let entities' = Map.insert serial entity $ entities gameData
    put gameData {entities = entities'}
    return entity

foldState :: (a -> GameState b) -> [a] -> GameState ()
foldState f list = do
    gameData <- get
    result <- liftIO $ foldM g gameData list
    put result
    return ()
    where
    g gameData' a = execStateT (f a) gameData'

getNextSerial :: GameState Integer
getNextSerial = do
    gameData <- get
    let serial = entitySerial gameData
    put gameData {entitySerial = serial + 1}
    return serial

getEntity :: Serial -> GameData -> Entity
getEntity serial gameData = entities gameData Map.! serial

data GameData = GameData {
    entitySerial :: Integer, 
    entities :: Map.Map Serial Entity, 
    physicsState :: PhysicsState,
    playerSerial :: Serial,
    transformState :: TransformState, 
    randomState :: StdGen, 
    renderFunctions :: Map.Map Serial (Float -> Entity -> GameState ()),
    simpleMovementState :: SimpleMovementState,
    scaleState :: ScaleState,
    space :: H.Space,
    tiles :: Map.Map String Picture,
    toBeRendered :: [RenderData]}

type GameState = StateT GameData IO

initialGameData :: IO GameData
initialGameData = do
    space' <- liftIO H.newSpace
    return GameData{
        entitySerial = 0, 
        entities = Map.empty, 
        physicsState = initialPhysicsState,
        playerSerial = -666,
        randomState = mkStdGen 1, 
        scaleState = initialScaleState,
        simpleMovementState = initialSimpleMovementState,
        transformState = initialTransformState,
        renderFunctions = Map.empty,
        space = space',
        tiles = Map.empty,
        toBeRendered = []}

removeComponent :: Component -> Entity -> GameState Entity
removeComponent component (Entity serial kind components) = do
    let entity = Entity serial kind $ Set.delete component components
    gameData <- get
    let entities' = Map.insert serial entity $ entities gameData
    put gameData{entities = entities'}
    return entity
