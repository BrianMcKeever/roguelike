--import qualified Data.Map as Map
--import Data.Maybe
--import qualified Data.Set as Set
--import qualified Data.Vector as Vector
--import EntityComponentSystem
import Components.Physics
import Components.Renderable
--import qualified Data.Vector as Vector
import GHC.Float
import GameState
import Graphics.Gloss.Data.Picture hiding (Point)
import Graphics.Gloss.Interface.Pure.Game hiding (Point)
import Linear.Affine
import Linear.V2
import Tiles

bucketHeight :: Int
bucketHeight = 1

bucketWidth :: Int
bucketWidth = 1

displayHeight :: Int
displayHeight = 704

displayWidth :: Int
displayWidth = 704

draw :: GameData -> Picture
draw gameData = pictures [area, entities]
    where
    area = renderArea (tiles gameData) (tiledMap gameData) (0, 0)
    physics' = physics gameData
    masks' = masks gameData
    renderData' = renderData gameData
    entities = renderEntities bucketWidth bucketHeight mapWidth physics' masks' renderData' displayWidth displayHeight (P (V2 0 0) :: Point V2 Double)

handleInput :: Event -> GameData -> GameData
handleInput _ gameData = gameData

main :: IO ()
main = do
    gameData <- initialGameData
    play (InWindow windowTitle (displayWidth, displayHeight) (50, 50)) white 60 gameData draw handleInput update

mapWidth :: Int
mapWidth = 200 * 16 * 4

mapHeight :: Int
mapHeight = 200 * 16 * 4

update :: Float -> GameData -> GameData
update tick gameData = gameData{physics = newPhysics}
    where
    tick' = float2Double tick
    masks' = masks gameData
    numberRelaxations = 10
    newPhysics = 
        physicsUpdate bucketWidth bucketHeight mapWidth mapHeight numberRelaxations tick' masks' $ physics gameData

windowTitle :: String
windowTitle = "Rogue Bard"
