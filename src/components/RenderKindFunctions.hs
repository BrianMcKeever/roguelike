module Components.RenderKindFunctions (
    renderKindFunctions
)
where
import qualified Data.Map.Lazy as Map
import Entities.Plants
import EntityComponentSystem
import GameState
import World

renderKindFunctions :: Map.Map Kind (Float -> GameState -> Entity -> GameState)
renderKindFunctions = Map.fromList [
    (tree, renderTree),
    (groundBrick, renderGround)
    ]
