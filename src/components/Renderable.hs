module Components.Renderable (
    addRenderable,
    basicRender,
    hasRenderable,
    normalScale,
    RenderData(..),
    ZIndex(..)
)
where
import Components.TransformAndPhysics
import Components.RenderableBase
import Control.Monad.State.Lazy
import qualified Data.Map.Lazy as Map
import Data.List.Ordered
import EntityComponentSystem
import GameState
import Graphics.Gloss.Game

addRenderable :: Entity -> (Float -> Entity -> GameState ()) -> GameState Entity
addRenderable entity f = do
    genericAddComponent renderableComponent entity
    gameData <- get
    let renderFunctions' = Map.insert entity f $ renderFunctions gameData
    put gameData{renderFunctions = renderFunctions'}
    return entity

basicRender :: ZIndex -> String -> Float -> Entity -> GameState ()
basicRender zindex tileName _ entity = do
    gameData <- get
    let tile = tiles gameData Map.! tileName
    position <- liftIO $ getPosition entity gameData
    let (x, y) = positionToPoint position
    let (scaleX, scaleY) = getScale gameData entity
    let tile' = translate x y $ scale scaleX scaleY tile
    --TODO  scaling all of the tiles every frame is probably bad
    let renderD = RenderData zindex tile'
    let renderData' = insertBag renderD $ toBeRendered gameData
    put gameData{toBeRendered = renderData'}

hasRenderable :: Entity -> GameData -> Bool
hasRenderable entity gameData = Map.member entity $ renderFunctions gameData

normalScale :: Float
normalScale = 4

removeRenderable :: Entity -> GameState ()
removeRenderable entity = do
    genericRemoveComponent renderableComponent entity
    gameData <- get
    let state' = Map.delete entity $ renderFunctions gameData

    put gameData{ renderFunctions = state'}

renderableComponent :: Component
renderableComponent = Component{
    hasComponent = genericHasComponent renderableComponent,
    nameComponent = createComponentName "renderable",
    removeComponent = removeRenderable
    }

