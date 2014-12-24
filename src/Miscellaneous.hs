module Miscellaneous (
    bouncingCirclesInitialData,
    trace
)
where
import Components.Physics
import Components.Renderable
--import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Tiled
import qualified Data.Vector as Vector
--import qualified Data.Vector.Mutable as Vector
import EntityComponentSystem
import GameState
import qualified Graphics.Gloss.Data.Picture as Gloss
import Linear.Affine
import Linear.V2
import System.Random
import Tiles

bouncingCirclesInitialData :: IO GameData
bouncingCirclesInitialData = do
    mapFile <- loadMapFile "assets/tile_map.tmx"
    tiles' <- loadTiles mapFile
    let radius = 2
    let circlePicture = Gloss.Circle radius
    entityPhysics' <- Vector.replicateM maxEntities createRandomEntityPhysics
    return GameData {
        masks = Vector.replicate maxEntities $ createMask [PhysicsComponent, RenderableComponent],
        physics = Physics entityPhysics' Set.empty Set.empty Set.empty Vector.empty,
        player = -666,
        randomState = mkStdGen 1,
        renderData = Vector.replicate maxEntities (RenderData Torso circlePicture),
        tiledMap = mapFile,
        tiles = tiles'
    }

createRandomEntityPhysics :: IO (EntityPhysics Double)
createRandomEntityPhysics = do
    let radius = 2
    let force' = V2 0 0
    let mass = 10
    let dimension = 200 * 16 * 4
    x <- randomRIO (0, dimension)
    y <- randomRIO (0, dimension)
    let oldPosition = P (V2 x y)
    xDelta <- randomRIO ((-10), 10)
    yDelta <- randomRIO ((-10), 10)
    let newPosition = P (V2 (x + xDelta) (y + yDelta))
    return $ EntityPhysics force' False False mass $ Circle oldPosition newPosition radius
