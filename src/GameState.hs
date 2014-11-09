module GameState (
    addComponent,
    createEntity,
    foldState,
    GameData(..),
    GameState,
    hasComponent,
    initialGameData
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

addComponent :: Component -> Entity -> GameState ()
addComponent component entity = do
    gameData <- get
    let components' = Map.insertWith Set.union entity (Set.singleton component) $ components gameData
    put gameData{components = components'}
  
createEntity :: GameState Entity
createEntity = do
    entity <- getNextSerial
    gameData <- get
    let entities' = Set.insert entity $ entities gameData
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

hasComponent :: Entity -> GameData -> Component -> Bool
hasComponent entity gameData component = result
    where
    components' = components gameData
    maybeComponents = Map.lookup entity components'
    result = maybe False (Set.member component) maybeComponents

data GameData = GameData {
    components :: ComponentData,
    entitySerial :: Integer, 
    entities :: Set.Set Entity, 
    physicsState :: PhysicsState,
    player :: Entity,
    randomState :: StdGen, 
    renderFunctions :: Map.Map Entity (Float -> Entity -> GameState ()),
    simpleMovementState :: SimpleMovementState,
    scaleState :: ScaleState,
    space :: H.Space,
    tiles :: Map.Map String Picture,
    toBeRendered :: [RenderData],
    transformComponents :: TransformComponents,
    transformState :: TransformState}

type GameState = StateT GameData IO

initialGameData :: IO GameData
initialGameData = do
    space' <- liftIO H.newSpace
    return GameData{
        components = initialComponentData,
        entitySerial = 0, 
        entities = Set.empty, 
        transformComponents = initialTransformComponents,
        physicsState = initialPhysicsState,
        player = -666,
        randomState = mkStdGen 1, 
        scaleState = initialScaleState,
        simpleMovementState = initialSimpleMovementState,
        transformState = initialTransformState,
        renderFunctions = Map.empty,
        space = space',
        tiles = Map.empty,
        toBeRendered = []}
