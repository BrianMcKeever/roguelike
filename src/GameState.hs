module GameState (
    GameData(..),
    GameState,
    initialGameData
)
where
import Control.Monad.State.Lazy
--import qualified Data.Map.Lazy as Map
--import qualified Data.Set as Set
--import qualified Data.Vector.Mutable as Vector
import EntityComponentSystem
--import Graphics.Gloss.Game
import System.Random

data GameData = GameData {
    masks :: Masks,
    --physicsComponents :: PhysicsComponents,
    player :: Entity,
    randomState :: StdGen
    }

type GameState = State GameData

initialGameData :: GameData
initialGameData = GameData{
        masks = initialMasks,
     --   physicsComponents = initialPhysics,
        player = -666,
        randomState = mkStdGen 1
    }
