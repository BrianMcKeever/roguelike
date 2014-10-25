module Components.Physics (
    addPhysics,
    createSquare,
    getBody,
    getVelocity,
    physicsComponent,
    PhysicsState,
    setVelocity
)
where
import Components.PhysicsBase
import Components.Transform
import qualified Data.Map.Lazy as Map
import Data.StateVar
import EntityComponentSystem
import GameState
import qualified Physics.Hipmunk as H

addPhysics :: H.Mass -> H.Moment -> H.ShapeType -> GameState -> Entity -> IO (Entity, GameState)
addPhysics mass moment shapeType gameState entity@(Entity serial _ _) = do
    let space' = space gameState
    body <- H.newBody mass moment
    H.spaceAdd space' body
    shape <- H.newShape body shapeType $ H.Vector 0 0
    H.spaceAdd space' shape
    -- I am assuming I will only be using simple shapes, so I've set the
    -- position offset to (0, 0)

    let physicsState' = Map.insert serial (PhysicsData body shape) $ physicsState gameState
    let (entity', gameState') = addComponent physicsComponent gameState entity
    let gameState'' = gameState'{physicsState = physicsState'}
    if hasComponent entity transformComponent
    then error "Add physics component before transformationComponent"
    else return (entity', gameState'')

createSquare :: Double -> Double -> H.ShapeType
createSquare width height = H.Polygon [ne, se, sw, nw]
    where
    halfWidth = width/2
    halfHeight = height/2
    ne = H.Vector halfWidth halfHeight
    se = H.Vector halfWidth (-halfHeight)
    nw = H.Vector (-halfWidth) halfHeight
    sw = H.Vector (-halfWidth) (-halfHeight)

getBody :: GameState -> Entity -> H.Body
getBody gameState (Entity serial _ _) = body
    where
    (PhysicsData body _) = physicsState gameState Map.! serial

getVelocity :: GameState -> Entity -> IO (H.Velocity)
getVelocity gameState entity = do
    let body = getBody gameState entity
    get $ H.velocity body

setVelocity :: H.Velocity -> GameState -> Entity -> IO ()
setVelocity velocity gameState entity = do
    let body = getBody gameState entity
    H.velocity body $= velocity
