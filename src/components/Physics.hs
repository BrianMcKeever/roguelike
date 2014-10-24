module Components.Physics (
    addPhysics,
    createSquare,
    getBody,
    physicsComponent,
    PhysicsState
)
where
import Components.PhysicsBase
import Components.Transform
import qualified Data.Map.Lazy as Map
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

{-
setAcceleration :: Acceleration -> GameState -> Entity -> GameState
setAcceleration acceleration gameState (Entity serial _ _) = gameState {physicsState = state'}
    where
    state = physicsState gameState
    (PhysicsData _ velocity) = Map.! serial state
    state' = Map.insert serial (PhysicsData acceleration velocity) state

setVelocity :: Velocity -> GameState -> Entity -> GameState
setVelocity velocity gameState (Entity serial _ _) = gameState {physicsState = state'}
    where
    state = physicsState gameState
    (PhysicsData acceleration _) = Map.! serial state
    state' = Map.insert serial (PhysicsData acceleration velocity) state
    -}
