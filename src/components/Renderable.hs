module Components.Renderable (
    renderableComponent
)
where
import EntityComponentSystem
import Graphics.Gloss.Game
import qualified Data.Map.Lazy as Map

renderableComponent :: Component
renderableComponent = Component 0 "renderable"

data RenderData = RenderData ZIndex Picture

type RenderableState = Map.Map Serial RenderData

type ZIndex = Int
