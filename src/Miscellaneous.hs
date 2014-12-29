module Miscellaneous (
    bouncingCirclesInitialData
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
import qualified Graphics.Gloss.Data.Color as Gloss2
import Linear.Affine
import Linear.V2
import System.Random
import Tiles

bouncingCirclesInitialData :: IO GameData
bouncingCirclesInitialData = do
    mapFile <- loadMapFile "assets/tile_map.tmx"
    tiles' <- loadTiles mapFile
    let radius = 32
    let circlePicture = Gloss.color Gloss2.rose $ Gloss.circleSolid radius
    let numberEntities = 30
    entityPhysics' <- Vector.replicateM numberEntities createRandomEntityPhysics
    let stationaryCircle = Vector.singleton $ EntityPhysics (V2 0 0) False False 10 $ Circle (P (V2 0 0)) (P (V2 0 0)) 5
    return GameData {
        masks = Vector.replicate (numberEntities + 1) $ createMask [PhysicsComponent, RenderableComponent],
        physics = Physics (entityPhysics' Vector.++ stationaryCircle) Set.empty Set.empty Set.empty Vector.empty,
        player = -666,
        randomState = mkStdGen 1,
        renderData = Vector.replicate (numberEntities + 1) (RenderData Torso circlePicture),
        tiledMap = mapFile,
        tiles = tiles'
    }

createRandomEntityPhysics :: IO (EntityPhysics Float)
createRandomEntityPhysics = do
    let radius = 32
    let force' = V2 0 0
    let mass = 10
    let dimension = 200 * 16 * 4
    let maxSpeed = 2
    x <- randomRIO (0, 64 * 4)
    y <- randomRIO (0, 64 * 4)
    let oldPosition = P (V2 x y)
    xDelta <- randomRIO ((-maxSpeed), maxSpeed)
    yDelta <- randomRIO ((-maxSpeed), maxSpeed)
    let newPosition = P (V2 (x + xDelta) (y + yDelta))
    --let newPosition = P (V2 0 0)
    return $ EntityPhysics force' False False mass $ Circle oldPosition newPosition radius
