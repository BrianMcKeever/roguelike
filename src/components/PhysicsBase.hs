module Components.PhysicsBase (
    initialPhysicsState,
    physicsComponent,
    PhysicsData(..),
    PhysicsState
)
where
import qualified Data.Map.Lazy as Map
import EntityComponentSystem
import qualified Physics.Hipmunk as H
import StringTable.Atom

initialPhysicsState :: PhysicsState
initialPhysicsState = Map.empty

physicsComponent :: Component
physicsComponent = Component 0 $ toAtom "physics"

data PhysicsData = PhysicsData H.Body H.Shape
-- Again, I'm assuming I will only be using simple shapes, and won't be using
-- any combined shapes.

type PhysicsState = Map.Map Serial PhysicsData
