--import qualified Data.Map as Map
--import Data.Maybe
--import qualified Data.Set as Set
--import qualified Data.Vector as Vector
--import EntityComponentSystem
import Components.Physics
import qualified Data.Vector as Vector
import GHC.Float
import GameState
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Game hiding (play)
import Graphics.Gloss.Interface.Pure.Game
import Tiles

draw :: GameData -> Picture
draw gameData = renderArea (tiles gameData) (tiledMap gameData) (0, 0)

handleInput :: Event -> GameData -> GameData
handleInput _ gameData = gameData

main :: IO ()
main = do
    gameData <- initialGameData
    play (InWindow windowTitle (windowWidth, windowHeight) (50, 50)) white 60 gameData draw handleInput update

update :: Float -> GameData -> GameData
update tick gameData = gameData{physicsDatas = newPhysics}
    where
    tick' = float2Double tick
    masks' = masks gameData
    physics = physicsDatas gameData
    (_, _, newPhysics) = physicsUpdate tick' $ Vector.zip masks' physics

windowTitle :: String
windowTitle = "Rogue Bard"

windowWidth :: Int
windowWidth = 704

windowHeight :: Int
windowHeight = 704
