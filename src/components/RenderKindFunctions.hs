module Components.RenderKindFunctions (
    renderKindFunctions
)
where
import qualified Data.Map.Lazy as Map
import Entities.Plants
import Entities.Player
import EntityComponentSystem
import GameState
import World

renderKindFunctions :: Map.Map Kind (Float -> GameState -> Entity -> IO GameState)
renderKindFunctions = Map.fromList [
    (tree, renderTree),
    (groundBrick, renderGround),
    (player, renderPlayer)
    ]
