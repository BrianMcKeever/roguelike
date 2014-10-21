module World (
    groundBrick,
    loadMap,
    tree,
    renderGround
)
where
import Components.Position
import Components.Renderable
import qualified Data.List as List
import Entities.Plants
import GameState
import EntityComponentSystem
import System.Random
import StringTable.Atom
import Tiles
--import Debug.Trace

createBrick :: Float -> Float -> GameState -> GameState
createBrick x y gameState = gameState5
    where
    (entity, gameState') = createEntity gameState groundBrick
    (entity', gameState'') = addPosition (x, y) gameState' entity
    gameState3 = snd $ addRenderable gameState'' entity'

    (roll, gameState4) = generateRandomBetween (0, 100) gameState3
    gameState5 = if roll > oddsOfTree
        then gameState4
        else createTree (x, y) gameState4

createRow :: Float -> GameState -> GameState
createRow y gameState = List.foldl' (flip (flip createBrick y)) gameState tileRange

generateRandomBetween :: (Int, Int) -> GameState -> (Int, GameState)
generateRandomBetween range gameState = (roll, gameState')
    where
    (roll, randomState') = randomR range $ randomState gameState
    gameState' = gameState{randomState = randomState'}

groundBrick :: Kind
groundBrick = toAtom "groundBrick"

loadMap :: GameState -> GameState
loadMap gameState =  List.foldl' (flip createRow) gameState tileRange

maximumCoordinate :: Float
maximumCoordinate = 100 * tileSize

minimumCoordinate :: Float
minimumCoordinate = (-10) * tileSize

oddsOfTree :: Int
oddsOfTree = 20

renderGround :: Float -> GameState -> Entity -> GameState
renderGround = basicRender Body "grass"

tileRange :: [Float]
tileRange = [minimumCoordinate, minimumCoordinate + tileSize..maximumCoordinate]

tileSize :: Float
tileSize = 16 * scaleFactor
