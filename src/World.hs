module World (
    loadMap,
    renderGround
)
where
import Components.Renderable
import Components.TransformAndPhysics
import Control.Monad.State.Lazy
import Entities.Plants
import GameState
import GHC.Float
import EntityComponentSystem
import qualified Physics.Hipmunk as H
import System.Random

createBrick :: Double -> Double -> GameState ()
createBrick x y = do
    entity <- createEntity
    addTransform (H.Vector x y) normalScale normalScale entity
    addRenderable entity renderGround

    --Todo  this doesn't belong here
    roll <- generateRandomBetween (0, 100)
    if roll > oddsOfTree
        then return ()
        else do
            createTree (H.Vector x y)
            return ()

createRow :: Double -> GameState ()
createRow y = foldState (flip createBrick y) tileRange

generateRandomBetween :: (Int, Int) -> GameState Int
generateRandomBetween range = do
    gameData <- get
    let (roll, randomState') = randomR range $ randomState gameData
    put gameData{randomState = randomState'}
    return roll

loadMap :: GameState ()
loadMap = foldState createRow tileRange

maximumCoordinate :: Double
maximumCoordinate = 5 * tileSize

minimumCoordinate :: Double
minimumCoordinate = (-5) * tileSize

oddsOfTree :: Int
oddsOfTree = 20

renderGround :: Float -> Entity -> GameState ()
renderGround = basicRender Earth "grass"

tileRange :: [Double]
tileRange = [minimumCoordinate, minimumCoordinate + tileSize..maximumCoordinate]

tileSize :: Double
tileSize = 16 * float2Double normalScale
