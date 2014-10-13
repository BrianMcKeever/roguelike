module World (
    createWorld, 
    World
)
where
import Components.Position
import qualified Data.Map as Map
import qualified Data.HashSet as Set
import EntityComponentSystem
import Graphics.Gloss.Game
import System.Random

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

initialWorld :: World
initialWorld = {entitySerial = 0, randomState = 0, entities = [], positionState = initialPositionState}

maximumCoordinate :: Int
maximumCoordinate = 100

minimumCoordinate :: Int
minimumCoordinate = (-100)

oddsOfTree :: Int
oddsOfTree = 20

data World = World {entitySerial :: Int, entities :: [Entity], positionState :: PositionState}
