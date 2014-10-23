module Components.Physics (
    addPhysics,
    getBody,
    physicsComponent,
    physicsState(..)
)
where
import Components.PhysicsBase
import Components.Transform
import qualified Data.Map.Lazy as Map
import Data.List.Ordered
import EntityComponentSystem
import GameState
import Graphics.Gloss.Game
import qualified Physics.Hipmunk as H

addPhysics :: H.Mass -> H.Moment -> H.ShapeType -> GameState -> Entity -> IO (Entity, GameState)
addPhysics gameState (Entity serial _ _) = do
    body <- H.newBody mass moment
    H.spaceAdd space body
    shape <- H.newShape body shapeType (0, 0)
    H.spaceAdd space shape
    -- I am assuming I will only be using simple shapes, so I've set the
    -- position offset to (0, 0)

    let physicsState = Map.insert serial (PhysicsData body shape) $ physicsState gameState
    let (entity', gameState') = addComponent physicsComponent gameState entity
    let gameState'' = gameState'{physicsState = physicsState}
    if hasComponent entity transformComponent
    then do
        -- if they have it, it's not in collision space, so we re-add it to make
        -- it collision space.
        position <- getPosition gameState entity --intentionally using old version of gameState
        let (entity'', gameState3) = removeTransform gameState'' entity'
        addTransform gameState3 entity'' position
    else return (entity', gameState'')

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
