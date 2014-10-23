module UpdateFunctions (
    updateFunctions
)
where
import Components.PhysicsBase
import Components.RenderableBase
import Components.TransformBase
import qualified Data.Map.Lazy as Map
import EntityComponentSystem
import GameState

updateFunctions :: Map.Map Component (Float -> GameState -> Entity -> GameState)
updateFunctions = Map.fromList  [
    (physicsComponent, pass),
    (transformComponent, pass),
    (renderableComponent, pass)
    ]
    where
    pass _ gameState _ = gameState
-- I decided to store my update functions in this dictionary instead of each
-- component because storing them in each component would cause circular imports
