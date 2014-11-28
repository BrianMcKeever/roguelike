module GameState (
    GameData(..),
    GameState,
    initialGameData
)
where
import Control.Monad.State.Lazy
--import qualified Data.Map.Lazy as Map
--import qualified Data.Set as Set
import Data.Tiled
--import qualified Data.Vector.Mutable as Vector
import EntityComponentSystem
--import Graphics.Gloss.Game
import System.Random

data GameData = GameData {
    masks :: Masks,
    --physicsComponents :: PhysicsComponents,
    player :: Entity,
    randomState :: StdGen,
    tiledMap :: TiledMap
    }

type GameState = State GameData

initialGameData :: IO GameData
initialGameData = do
    mapFile <- loadMapFile "assets/tile_map.tmx"
    return GameData {
        masks = initialMasks,
     --   physicsComponents = initialPhysics,
        player = -666,
        randomState = mkStdGen 1,
        tiledMap = mapFile
    }
