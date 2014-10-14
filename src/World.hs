module World (
    GameState,
    initialGameState,
    loadMap
)
where
import Components.Position
import qualified Data.Map as Map
import qualified Data.List as List
import EntityComponentSystem
import Graphics.Gloss.Game
import System.Random

createBrick :: Map.Map String Picture -> Int -> Int -> GameState -> GameState
createBrick tiles x y gameState = gameState''{positionState = positionState'}
    where
    (roll, gameState') = generateRandomBetween (0, 100) gameState
    tile = if roll < oddsOfTree
        then tiles Map.! "grass"
        else tiles Map.! "tree"
    (entity, gameState'') = createEntity gameState'
    positionState' = snd $ addPosition (x, y) (positionState gameState) entity

createEntity :: GameState -> (Entity, GameState)
createEntity gameState = (entity, gameState') 
    where
    serial = entitySerial gameState
    entity = Entity serial emptyComponents
    entities' = Map.insert serial entity $ entities gameState
    gameState' = gameState {entitySerial = serial + 1, entities = entities'}

createRow :: Map.Map String Picture -> Int -> GameState -> GameState
createRow tiles y gameState = List.foldl' (flip (flip (createBrick tiles) y)) gameState [minimumCoordinate.. maximumCoordinate] 

data GameState = GameState {entitySerial :: Int, entities :: (Map.Map Serial Entity), positionState :: PositionState, randomState :: StdGen}

generateRandomBetween ::(Int, Int) -> GameState -> (Int, GameState)
generateRandomBetween range gameState = (roll, gameState')
    where
    (roll, randomState') = randomR range $ randomState gameState
    gameState' = gameState{randomState = randomState'}

initialGameState :: GameState
initialGameState = GameState{entitySerial = 0, entities = Map.empty, randomState = mkStdGen 0, positionState = initialPositionState}

loadMap :: GameState -> Map.Map String Picture -> GameState
loadMap gameState tiles =  List.foldl' (flip (createRow tiles)) gameState [minimumCoordinate.. maximumCoordinate] 

maximumCoordinate :: Int
maximumCoordinate = 100

minimumCoordinate :: Int
minimumCoordinate = (-100)

oddsOfTree :: Int
oddsOfTree = 20
