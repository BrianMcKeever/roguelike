module GameState (
    Component(..),
    createEntity,
    createComponentName,
    genericAddComponent,
    genericHasComponent,
    genericRemoveComponent,
    foldState,
    GameData(..),
    GameState,
    initialGameData,
    removeEntity
)
where
import Components.PhysicsBase
import Components.RenderableBase
import Components.SimpleMovementBase
import Components.TransformBase
import Control.Monad.State.Lazy
import Data.IORef
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import EntityComponentSystem
import Graphics.Gloss.Game
import qualified Physics.Hipmunk as H
import StringTable.Atom
import System.Random

data Component = Component {
    hasComponent :: Entity -> GameData -> Bool,
    nameComponent :: ComponentName,
    removeComponent :: Entity -> GameState ()
}

instance Eq Component where
    x == y = nameComponent x == nameComponent y

instance Ord Component where
    x `compare` y = nameComponent x `compare` nameComponent y

type ComponentData = Map.Map Entity (Set.Set Component)

type ComponentName = Atom

createEntity :: GameState Entity
createEntity = do
    entity <- getNextSerial
    gameData <- get
    let entities' = Set.insert entity $ entities gameData
    put gameData {entities = entities'}
    return entity

createComponentName :: String -> ComponentName
createComponentName = toAtom

foldState :: (a -> GameState b) -> [a] -> GameState ()
foldState f list = do
    gameData <- get
    result <- liftIO $ foldM g gameData list
    put result
    return ()
    where
    g gameData' a = execStateT (f a) gameData'

data GameData = GameData {
    components :: ComponentData,
    doaCollisions :: IORef [(H.Shape, H.Shape)],
    entitySerial :: Integer, 
    entities :: Set.Set Entity, 
    etherealCollisions :: IORef [(H.Shape, H.Shape)],
    physicsState :: PhysicsState,
    player :: Entity,
    randomState :: StdGen, 
    renderFunctions :: Map.Map Entity (Float -> Entity -> GameState ()),
    scaleState :: ScaleState,
    shapes :: ShapeState,
    simpleMovementState :: SimpleMovementState,
    space :: H.Space,
    tiles :: Map.Map String Picture,
    toBeRendered :: [RenderData],
    transformComponents :: TransformComponents,
    transformState :: TransformState}

type GameState = StateT GameData IO

genericAddComponent :: Component -> Entity -> GameState ()
genericAddComponent component entity = do
    gameData <- get
    let components' = Map.insertWith Set.union entity (Set.singleton component) $ components gameData
    put gameData{components = components'}
  
genericHasComponent :: Component -> Entity -> GameData -> Bool
genericHasComponent component entity gameData = result
    where
    components' = components gameData
    maybeComponents = Map.lookup entity components'
    result = maybe False (Set.member component) maybeComponents

genericRemoveComponent :: Component -> Entity -> GameState ()
genericRemoveComponent component entity = do
    gameData <- get
    let components' = components gameData
    put gameData{components = Map.adjust (Set.delete component) entity components'}

getComponents :: Entity -> GameData -> Set.Set Component
getComponents entity gameData = components gameData Map.! entity

getNextSerial :: GameState Integer
getNextSerial = do
    gameData <- get
    let serial = entitySerial gameData
    put gameData {entitySerial = serial + 1}
    return serial

initialGameData :: IO GameData
initialGameData = do
    etherealCollisions' <- newIORef []
    doaCollisions' <- newIORef []
    space' <- liftIO H.newSpace
    return GameData{
        components = initialComponentData,
        doaCollisions = doaCollisions',
        entitySerial = 0, 
        entities = Set.empty, 
        etherealCollisions = etherealCollisions',
        transformComponents = initialTransformComponents,
        physicsState = initialPhysicsState,
        player = -666,
        randomState = mkStdGen 1, 
        scaleState = initialScaleState,
        shapes = initialShapeState,
        simpleMovementState = initialSimpleMovementState,
        transformState = initialTransformState,
        renderFunctions = Map.empty,
        space = space',
        tiles = Map.empty,
        toBeRendered = []}

initialComponentData :: ComponentData
initialComponentData = Map.empty

removeEntity :: Entity -> GameState ()
removeEntity entity = do
    gameData <- get
    foldState f $ Set.toList $ getComponents entity gameData
    gameData2 <- get
    put gameData2{entities = Set.delete entity $ entities gameData2}
    where
    f component = removeComponent component entity
