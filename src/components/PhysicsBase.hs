module Components.PhysicsBase (
    initialPhysicsState,
    initialShapeState,
    physicsComponent,
    PhysicsData(..),
    PhysicsState,
    ShapeState
)
where
import qualified Data.Map.Lazy as Map
import EntityComponentSystem
import qualified Physics.Hipmunk as H

initialPhysicsState :: PhysicsState
initialPhysicsState = Map.empty

initialShapeState :: ShapeState
initialShapeState = Map.empty

physicsComponent :: Component
physicsComponent = createComponent "physics"

data PhysicsData = PhysicsData H.Body H.Shape Bool
-- Again, I'm assuming I will only be using simple shapes, and won't be using
-- any combined shapes.

type PhysicsState = Map.Map Entity PhysicsData

type ShapeState = Map.Map H.Shape Entity
