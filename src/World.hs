module World (
    GameState(..),
    initialGameState,
    loadMap,
    updateFunctions
)
where
import Components.Position
import Components.Renderable
import qualified Data.Map as Map
import qualified Data.List as List
import EntityComponentSystem
import Graphics.Gloss.Game
import System.Random

createBrick :: Int -> Int -> GameState -> GameState
createBrick x y gameState = gameState''{positionState = positionState'}
    where
    (roll, gameState') = generateRandomBetween (0, 100) gameState
    {-
    tile = if roll < oddsOfTree
        then tiles Map.! "grass"
        else tiles Map.! "tree"
        -}
    (entity, gameState'') = createEntity gameState'
    positionState' = snd $ addPosition (x, y) (positionState gameState) entity

createEntity :: GameState -> (Entity, GameState)
createEntity gameState = (entity, gameState') 
    where
    serial = entitySerial gameState
    entity = Entity serial emptyComponents
    entities' = Map.insert serial entity $ entities gameState
    gameState' = gameState {entitySerial = serial + 1, entities = entities'}

createRow :: Int -> GameState -> GameState
createRow y gameState = List.foldl' (flip (flip createBrick y)) gameState [minimumCoordinate.. maximumCoordinate] 

data GameState = GameState {entitySerial :: Int, entities :: (Map.Map Serial Entity), positionState :: PositionState, randomState :: StdGen}

generateRandomBetween ::(Int, Int) -> GameState -> (Int, GameState)
generateRandomBetween range gameState = (roll, gameState')
    where
    (roll, randomState') = randomR range $ randomState gameState
    gameState' = gameState{randomState = randomState'}

initialGameState :: GameState
initialGameState = GameState{entitySerial = 0, entities = Map.empty, randomState = mkStdGen 0, positionState = initialPositionState}

loadMap :: GameState -> GameState
loadMap gameState =  List.foldl' (flip createRow) gameState [minimumCoordinate.. maximumCoordinate] 

maximumCoordinate :: Int
maximumCoordinate = 100

minimumCoordinate :: Int
minimumCoordinate = (-100)

oddsOfTree :: Int
oddsOfTree = 20

updateFunctions :: Map.Map Component (GameState -> Entity -> GameState)
updateFunctions = Map.fromList  [
    (positionComponent, pass),
    (renderableComponent, pass)
    ]
    where
    pass gameState _ = gameState
-- I decided to store my update functions in this dictionary instead of each
-- component because storing them in each component would cause circular imports
