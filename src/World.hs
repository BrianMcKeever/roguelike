module World (
    loadMap
)
where
import Components.Position
--import Components.Renderable
import GameState
import qualified Data.Map as Map
import qualified Data.List as List
import EntityComponentSystem
import System.Random

createBrick :: Float -> Float -> GameState -> GameState
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

createRow :: Float -> GameState -> GameState
createRow y gameState = List.foldl' (flip (flip createBrick y)) gameState [minimumCoordinate.. maximumCoordinate] 

generateRandomBetween ::(Int, Int) -> GameState -> (Int, GameState)
generateRandomBetween range gameState = (roll, gameState')
    where
    (roll, randomState') = randomR range $ randomState gameState
    gameState' = gameState{randomState = randomState'}

loadMap :: GameState -> GameState
loadMap gameState =  List.foldl' (flip createRow) gameState [minimumCoordinate.. maximumCoordinate] 

maximumCoordinate :: Float
maximumCoordinate = 100

minimumCoordinate :: Float
minimumCoordinate = (-100)

oddsOfTree :: Int
oddsOfTree = 20
