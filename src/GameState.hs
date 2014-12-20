module GameState (
    GameData(..),
    GameState,
    initialGameData
)
where
import Components.Physics
import Components.Renderable
import Control.Monad.State.Lazy
--import qualified Data.Map.Lazy as Map
--import qualified Data.Set as Set
import Data.Tiled
import qualified Data.Vector as Vector
--import qualified Data.Vector.Mutable as Vector
import EntityComponentSystem
import Graphics.Gloss.Game
import System.Random
import Tiles

data GameData = GameData {
    masks :: Masks,
    player :: Entity,
    physics :: Physics Double,
    randomState :: StdGen,
    renderData :: Vector.Vector RenderData,
    tiledMap :: TiledMap,
    tiles :: Vector.Vector Picture
    }

type GameState = State GameData

initialGameData :: IO GameData
initialGameData = do
    mapFile <- loadMapFile "assets/tile_map.tmx"
    tiles' <- loadTiles mapFile
    return GameData {
        masks = initialMasks,
        physics = initialPhysics,
        player = -666,
        randomState = mkStdGen 1,
        renderData = initialRenderData,
        tiledMap = mapFile,
        tiles = tiles'
    }
