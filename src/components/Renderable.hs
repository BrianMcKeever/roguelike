module Components.Renderable (
    addRenderable,
    basicRender,
    renderableComponent,
    RenderData(..),
    ZIndex(..)
)
where
import Components.Position
import Components.RenderableBase
import qualified Data.Map.Lazy as Map
import Data.List.Ordered
import EntityComponentSystem
import GameState
import Graphics.Gloss.Game

addRenderable :: GameState -> Entity -> (Entity, GameState)
addRenderable gameState entity = (entity', gameState')
    where
    (entity', gameState') = addComponent gameState entity renderableComponent

basicRender :: ZIndex -> String -> Float -> GameState -> Entity -> GameState
basicRender zindex tileName _ gameState entity = gameState'
    where
    tile = tiles gameState Map.! tileName
    (x, y) = getPosition gameState entity
    tile' = translate x y tile
    renderD = RenderData zindex tile'
    renderData' = insertBag renderD $ toBeRendered gameState
    gameState' = gameState{toBeRendered = renderData'}
