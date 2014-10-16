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
import StringTable.Atom
import System.Random

createBrick :: Float -> Float -> GameState -> GameState
createBrick x y gameState = gameState4
    where
    (entity, gameState') = createEntity gameState groundBrick
    (entity', gameState'') = addPosition (x, y) gameState' entity

    (roll, gameState3) = generateRandomBetween (0, 100) gameState''
    gameState4 = if roll > oddsOfTree
        then gameState3
        else createTree (x, y) gameState3

createEntity :: GameState -> Kind -> (Entity, GameState)
createEntity gameState kind = (entity, gameState') 
    where
    serial = entitySerial gameState
    entity = Entity serial kind emptyComponents
    entities' = Map.insert serial entity $ entities gameState
    gameState' = gameState {entitySerial = serial + 1, entities = entities'}

createRow :: Float -> GameState -> GameState
createRow y gameState = List.foldl' (flip (flip createBrick y)) gameState [minimumCoordinate.. maximumCoordinate] 

createTree :: Point -> GameState -> GameState
createTree point gameState = gameState''
    where
    (entity, gameState') = createEntity gameState tree
    (entity', gameState'') = addPosition point gameState' entity

generateRandomBetween ::(Int, Int) -> GameState -> (Int, GameState)
generateRandomBetween range gameState = (roll, gameState')
    where
    (roll, randomState') = randomR range $ randomState gameState
    gameState' = gameState{randomState = randomState'}

groundBrick :: Kind
groundBrick = toAtom "groundBrick"

loadMap :: GameState -> GameState
loadMap gameState =  List.foldl' (flip createRow) gameState [minimumCoordinate.. maximumCoordinate] 

maximumCoordinate :: Float
maximumCoordinate = 100

minimumCoordinate :: Float
minimumCoordinate = (-100)

oddsOfTree :: Int
oddsOfTree = 20

tree :: Kind
tree = toAtom "tree"
