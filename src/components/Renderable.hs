module Components.Renderable (
    renderableComponent
)
where
import EntityComponentSystem
import Graphics.Gloss.Game
import qualified Data.Map.Lazy as Map
import StringTable.Atom

renderableComponent :: Component
renderableComponent = Component 0 $ toAtom "renderable"

data RenderData = RenderData ZIndex Picture

type RenderableState = Map.Map Serial RenderData

data ZIndex = Earth | Blood | Ground | Entity | Effect
