module Components.Physics (
    Collision,
    createCollision,
    PhysicsData(..),
    PhysicsDatas,
    physicsMask,
    physicsUpdate,
    initialPhysics,
    Shape(..)
)
where
import EntityComponentSystem
import qualified Data.Vector as Vector
import Linear.V2

createCollision :: Entity -> Entity -> Collision
createCollision a b 
    | a < b     = (a, b)
    | otherwise = (b, a)

type Collision = (Entity, Entity) 
-- A collision between a and b. a < b

type Collisions = Vector.Vector Collision

eulerIntegration :: Double -> Vector.Vector (Mask, PhysicsData) -> PhysicsDatas
eulerIntegration tick input = onlyMap (maskHas physicsMask) integrate input
--I'm using eulerIntegration because it is simple. It is not physically 
--accurate because we accumulate small errors due to not having an infinitely
--small time step.
    where
    v2Tick = V2 tick tick
    integrate physics@(PhysicsData force' _ _ _ mass' position' _ velocity') =
        physics{ 
            position = position' + velocity' * v2Tick,
            velocity = velocity' + (force' / V2 mass' mass') * v2Tick
            }

initialPhysics :: PhysicsDatas
initialPhysics = Vector.replicate maxEntities d
    where
    d = PhysicsData (V2 0 0) False False False 1 (V2 0 0) (Circle 0) (V2 0 0)

data PhysicsData = PhysicsData {
    force :: V2 Double,
    isDeadOnImpact :: Bool, --whether it gets "destroyed" on first contact
    isStatic :: Bool, 
    isTrigger :: Bool, 
    mass :: Double,
    position :: V2 Double,
    shape :: Shape,
    velocity :: V2 Double
    }

type PhysicsDatas = Vector.Vector PhysicsData

physicsMask :: Mask
physicsMask = componentToMask PhysicsComponent

physicsUpdate :: Double -> 
    Vector.Vector (Mask, PhysicsData) -> 
    (Collisions, Collisions, PhysicsDatas) --newCollisions, oldCollisions
physicsUpdate tick input = (Vector.empty, Vector.empty, newPhysics)
    where
    newPhysics = eulerIntegration tick input

data Shape = Circle Double -- radius
    | AABB Double Double -- length width
