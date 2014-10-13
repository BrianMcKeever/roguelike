module World (
    createEntity,
--    createWorld, 
    GameState,
    initialGameState,
--    initialWorld,
    World
)
where
--import Components.Position
import Control.Monad.State.Lazy
import qualified Data.Map as Map
import qualified Data.HashSet as Set
import EntityComponentSystem
import Graphics.Gloss.Game
import System.Random

{-
generateRandomBetween ::(Int, Int) -> World -> (Int, World)
generateRandomBetween range world = (roll, world')
    where
    (roll, randomState') = randomR range $ randomState world
    world' = world{randomState = randomState'}

createBrick :: Map.Map String Picture -> Int -> Int -> World -> (Entity, World)
createBrick tiles x y world = addPosition (x, y) world'' entity
    where
    (roll, world') = generateRandomBetween (0, 100) world
    tile = if roll < oddsOfTree
        then tiles Map.! "grass"
        else tiles Map.! "tree"
    (entity, world'') = createEntity world'

createEntity :: World -> (Entity, World)
createEntity world = createEntityWithComponents world $ componentsFromList []

createEntityWithComponents :: World -> Components -> (Entity, World)
createEntityWithComponents world components = (entity, world') 
    where
    serial = entitySerial world
    entity = Entity serial components
    entities' = entity : entities world
    world' = world {entitySerial = serial + 1, entities = entities'}

createRow :: Map.Map String Picture -> Int -> World -> World
createRow tiles y world = map (flip (createBrick tiles world) y) [minimumCoordinate.. maximumCoordinate]

createWorld :: Map.Map String Picture -> IO World
createWorld tiles = do
    let positionState' = createPositionState
    bricks <- map (flip (createRow tiles) positionState') [minimumCoordinate.. maximumCoordinate]
    return World {entities = bricks, positionState = positionState'}

maximumCoordinate :: Int
maximumCoordinate = 100

minimumCoordinate :: Int
minimumCoordinate = (-100)

oddsOfTree :: Int
oddsOfTree = 20
--}

createEntity :: World Entity
createEntity = do
    gameState <- get
    let serial = entitySerial gameState
    let entity = Entity serial $ componentsFromList []
    let entities' = Map.insert serial entity $ entities gameState
    put gameState {entitySerial = serial + 1, entities = entities'}
    return entity

data GameState = GameState {entitySerial :: Int, entities :: (Map.Map Serial Entity)}--, positionState :: PositionState}

initialGameState :: GameState
initialGameState = GameState{entitySerial = 0, entities = Map.empty} --randomState = 0, entities = [], positionState = initialPositionState}

--initialWorld :: World GameState
--initialWorld = runState get initialGameState

type World = State GameState 
