module World (
    groundBrick,
    loadMap,
    tree,
    renderGround
)
where
import Components.Transform
import Components.Renderable
import Control.Monad
import Entities.Plants
import GameState
import GHC.Float
import EntityComponentSystem
import qualified Physics.Hipmunk as H
import System.Random
import StringTable.Atom
import Tiles
--import Debug.Trace

createBrick :: Double -> Double -> GameState -> IO GameState
createBrick x y gameState = do
    let (entity, gameState') = createEntity gameState groundBrick
    (entity', gameState'') <- addTransform (H.Vector x y) gameState' entity
    let gameState3 = snd $ addRenderable gameState'' entity'

    let (roll, gameState4) = generateRandomBetween (0, 100) gameState3
    if roll > oddsOfTree
        then return gameState4
        else createTree (H.Vector x y) gameState4

createRow :: Double -> GameState -> IO GameState
createRow y gameState = foldM (flip (flip createBrick y)) gameState tileRange

generateRandomBetween :: (Int, Int) -> GameState -> (Int, GameState)
generateRandomBetween range gameState = (roll, gameState')
    where
    (roll, randomState') = randomR range $ randomState gameState
    gameState' = gameState{randomState = randomState'}

groundBrick :: Kind
groundBrick = toAtom "groundBrick"

loadMap :: GameState -> IO GameState
loadMap gameState = foldM (flip createRow) gameState tileRange

maximumCoordinate :: Double
maximumCoordinate = 100 * tileSize

minimumCoordinate :: Double
minimumCoordinate = (-5) * tileSize

oddsOfTree :: Int
oddsOfTree = 20

renderGround :: Float -> GameState -> Entity -> IO GameState
renderGround = basicRender Earth "grass"

tileRange :: [Double]
tileRange = [minimumCoordinate, minimumCoordinate + tileSize..maximumCoordinate]

tileSize :: Double
tileSize = 16 * float2Double scaleFactor
