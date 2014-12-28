module Game (
    displayWidth,
    displayHeight,
    draw,
    handleInput,
    update
) where
--import qualified Data.Map as Map
--import Data.Maybe
--import qualified Data.Set as Set
--import qualified Data.Vector as Vector
--import EntityComponentSystem
import Components.Physics
import Components.Renderable
import qualified Data.Vector as Vector
import GHC.Float
import GameState
import Graphics.Gloss.Data.Picture hiding (Point)
import Graphics.Gloss.Interface.Pure.Game hiding (Point)
import Linear.Affine
import Linear.V2
import Tiles

bucketHeight :: Int
bucketHeight = ceiling $ 1 * pixelScale

bucketWidth :: Int
bucketWidth = ceiling $ 1 * pixelScale

displayHeight :: Int
displayHeight = 704

displayWidth :: Int
displayWidth = 704

draw :: GameData -> Picture
draw gameData = if Vector.length space' == 0 then Blank else pictures [area, entities]
    where
    center = V2 5 5 :: V2 Float
    space' = space $ physics gameData
    area = renderArea (tiles gameData) (tiledMap gameData) center
    physics' = physics gameData
    masks' = masks gameData
    renderData' = renderData gameData
    entities = renderEntities bucketWidth bucketHeight mapWidth physics' masks' renderData' displayWidth displayHeight (P (V2 0 0))

handleInput :: Event -> GameData -> GameData
handleInput _ gameData = gameData

mapWidth :: Int
mapWidth = 200 * 16 * 4

mapHeight :: Int
mapHeight = 200 * 16 * 4

update :: Float -> GameData -> GameData
update tick gameData = gameData{physics = newPhysics}
    where
    masks' = masks gameData
    numberRelaxations = 10
    newPhysics = 
        physicsUpdate bucketWidth bucketHeight mapWidth mapHeight numberRelaxations tick masks' $ physics gameData
