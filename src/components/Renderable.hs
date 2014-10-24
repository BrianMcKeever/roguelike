module Components.Renderable (
    addRenderable,
    basicRender,
    renderableComponent,
    RenderData(..),
    ZIndex(..)
)
where
import Components.Transform
import Components.RenderableBase
import qualified Data.Map.Lazy as Map
import Data.List.Ordered
import EntityComponentSystem
import GameState
import Graphics.Gloss.Game

addRenderable :: GameState -> Entity -> (Entity, GameState)
addRenderable = addComponent renderableComponent

basicRender :: ZIndex -> String -> Float -> GameState -> Entity -> IO GameState
basicRender zindex tileName _ gameState entity = do
    let tile = tiles gameState Map.! tileName
    (x, y) <- fmap positionToPoint $ getPosition gameState entity
    let (scaleX, scaleY) = getScale gameState entity
    let tile' = translate x y $ scale scaleX scaleY tile
    let renderD = RenderData zindex tile'
    let renderData' = insertBag renderD $ toBeRendered gameState
    return gameState{toBeRendered = renderData'}
