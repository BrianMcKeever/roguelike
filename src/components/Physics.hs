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
import Control.Monad.State.Lazy
import qualified Data.Map.Lazy as Map
import qualified Data.StateVar as S
import EntityComponentSystem
import GameState
import qualified Physics.Hipmunk as H

addPhysics :: H.Mass -> H.Moment -> H.ShapeType -> Entity -> GameState Entity
addPhysics mass moment shapeType entity@(Entity serial _ _) = do
    gameData <- get
    let space' = space gameData
    body <- liftIO $ H.newBody mass moment
    liftIO $ H.spaceAdd space' body
    shape <- liftIO $ H.newShape body shapeType $ H.Vector 0 0
    liftIO $ H.spaceAdd space' shape
    -- I am assuming I will only be using simple shapes, so I'm defaulting the
    -- position offset to (0, 0)

    let physicsState' = Map.insert serial (PhysicsData body shape) $ physicsState gameData
    put gameData{physicsState = physicsState'}

    entity2 <- addComponent physicsComponent entity
    if hasComponent entity transformComponent
    then error "Add physics component before transformationComponent"
    else return entity2

createSquare :: Double -> Double -> H.ShapeType
createSquare width height = H.Polygon [ne, se, sw, nw]
    where
    halfWidth = width/2
    halfHeight = height/2
    ne = H.Vector halfWidth halfHeight
    se = H.Vector halfWidth (-halfHeight)
    nw = H.Vector (-halfWidth) halfHeight
    sw = H.Vector (-halfWidth) (-halfHeight)

getBody :: GameData -> Entity -> H.Body
getBody gameData (Entity serial _ _) = body
    where
    (PhysicsData body _) = physicsState gameData Map.! serial

getVelocity :: GameData -> Entity -> IO H.Velocity
getVelocity gameData entity = do
    let body = getBody gameData entity
    S.get $ H.velocity body

setVelocity :: H.Velocity -> Entity -> GameState ()
setVelocity velocity entity = do
    gameData <- get
    let body = getBody gameData entity
    liftIO $ H.velocity body S.$= velocity
