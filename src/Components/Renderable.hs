module Components.Renderable (
    --basicRender,
    initialRenderData,
    RenderData(..),
    renderEntities,
    renderableMask,
    ZIndex(..)
)
where
import Components.Physics
import Control.Monad.ST
import Data.Vector as Vector
import Data.Vector.Algorithms.Intro
import EntityComponentSystem
import Graphics.Gloss.Data.Picture hiding (Point, Vector)
import Linear.Affine
import Linear.V2
import Prelude as Prelude hiding ((++), concat, filter, foldl, init, last, length, map, maximum, minimum, replicate, sequence, span, sum, unzip, zip) 

{-
basicRender :: ZIndex -> String -> Float -> Entity -> GameState ()
basicRender zindex tileName _ entity = do
    gameData <- get
    let tile = tiles gameData Map.! tileName
    position <- liftIO $ getPosition entity gameData
    let (x, y) = positionToPoint position
    let (scaleX, scaleY) = getScale gameData entity
    let tile' = translate x y $ scale scaleX scaleY tile
    let renderD = RenderData zindex tile'
    let renderData' = insertBag renderD $ toBeRendered gameData
    put gameData{toBeRendered = renderData'}
-}

initialRenderData :: Vector RenderData
initialRenderData = replicate maxEntities (RenderData Torso Blank)

data RenderData = RenderData ! ZIndex ! Picture

instance Eq RenderData where
    (RenderData index1 _) == (RenderData index2 _) = index1 == index2

instance Ord RenderData where
    compare (RenderData index1 _) (RenderData index2 _) = compare index1 index2

renderEntities :: 
    Int -> Int -> Int -> Physics Float -> Masks -> Vector RenderData -> Int -> Int -> Point V2 Float -> Picture
renderEntities bucketWidth bucketHeight mapWidth physics' masks' renderData displayWidth displayHeight center = result
    -- Grab the entities in the buckets covering the screen.
    -- Filter ids that don't have Renderable component.
    -- Map entities to get their pictures.
    where
    entities = getEntitiesInBox bucketWidth bucketHeight mapWidth physics' displayWidth displayHeight center
    renderable = filter (\ entity -> hasMask (masks' ! fromIntegral entity)  renderableMask) entities
    renderData' = map getRenderData renderable

    getRenderData :: Entity -> RenderData
    getRenderData entity = renderD
        where
        (RenderData zIndex picture) = renderData ! fromIntegral entity
        (P (V2 x y)) = getCenter $ shape $ entityPhysics physics' ! fromIntegral entity
        translatedPicture = Translate x (-y) picture
        renderD = RenderData zIndex translatedPicture

    sorted = runST $ do
        thawed <- thaw renderData'
        sort thawed
        freeze thawed
    result = pictures $ toList $ map (\ (RenderData _ p) -> p) sorted

renderableMask :: Mask
renderableMask = componentToMask RenderableComponent

data ZIndex = Ground | Blood | Feet | Legs | Torso | Head | Sky deriving (Eq, Ord)
