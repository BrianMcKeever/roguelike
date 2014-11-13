module Components.TransformAndPhysics (
    addPhysics,
    addTransform,
    collisionTypeDOA,
    collisionTypeEthereal,
    collisionTypeNormal,
    createSquare,
    getBody,
    getPosition,
    getScale,
    hasTransform,
    getVelocity,
    hasPhysics,
    initializePhysics,
    positionToPoint,
    Position,
    setPosition,
    setVelocity,
    updatePhysics,
    withinBox
)
where
import Components.PhysicsBase
import Control.Monad.State.Lazy
import Data.IORef
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import qualified Data.StateVar as S
import EntityComponentSystem
import GameState
import GHC.Float
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Game
import qualified Physics.Hipmunk as H

addPhysics :: H.CollisionType -> H.Mass -> H.Moment -> H.ShapeType -> Entity -> Bool -> GameState Entity
addPhysics collisionType' mass moment shapeType entity isStatic = do
    addComponent physicsComponent entity
    gameData <- get
    let space' = space gameData
    body <- if isStatic
        then liftIO $ H.newBody H.infinity H.infinity
            --TODO when it is possible to add static bodies, we need to add them
        else liftIO $ H.newBody mass moment
    liftIO $ H.spaceAdd space' body
    shape <- liftIO $ H.newShape body shapeType $ H.Vector 0 0
    -- I am assuming I will only be using simple shapes, so I'm defaulting the
    -- position offset to (0, 0)

    liftIO $ H.collisionType shape S.$= collisionType'
    liftIO $ if isStatic
    then H.spaceAdd space' $ H.Static shape
    else H.spaceAdd space' shape

    let physicsState' = Map.insert entity (PhysicsData body shape isStatic) $ physicsState gameData
    let shapes' = Map.insert shape entity $ shapes gameData
    put gameData{physicsState = physicsState', shapes = shapes'}

    if hasTransform entity gameData
    then error "Add physics component before transformationComponent"
    else return entity

addTransform :: H.Position -> Float -> Float -> Entity -> GameState Entity
addTransform coordinate scaleX scaleY entity = do
    gameData <- get
    let components' = Set.insert entity $ transformComponents gameData
    put gameData{transformComponents = components'}
    setPosition coordinate entity
    setScale scaleX scaleY entity
    return entity

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
getBody gameData entity = body
    where
    (PhysicsData body _ _) = physicsState gameData Map.! entity

getPosition :: Entity -> GameData -> IO H.Position
getPosition entity gameData = do
    let (PhysicsData body _ _) = physicsState gameData Map.! entity
    if hasPhysics gameData entity
    then S.get $ H.position body
    else return $ transformState gameData Map.! entity

getScale :: GameData -> Entity -> (Float, Float)
getScale gameData entity = scaleState gameData Map.! entity

getVelocity :: GameData -> Entity -> IO H.Velocity
getVelocity gameData entity = do
    let body = getBody gameData entity
    S.get $ H.velocity body

hasPhysics :: GameData -> Entity -> Bool
hasPhysics = hasComponent physicsComponent

hasTransform :: Entity -> GameData -> Bool
hasTransform entity gameData = Set.member entity $ transformComponents gameData

initializePhysics :: GameState ()
initializePhysics = do
    gameData <- get
    let collisions' = collisions gameData
    liftIO $ H.setDefaultCollisionHandler (space gameData) $ H.Handler
        (Just $ beginHandler' True collisions') Nothing Nothing Nothing

    let doaCollisions' = doaCollisions gameData
    liftIO $ H.addCollisionHandler (space gameData) collisionTypeDOA collisionTypeNormal $ H.Handler
        (Just $ beginHandler' True doaCollisions') Nothing Nothing Nothing

    let etherealCollisions' = etherealCollisions gameData
    liftIO $ H.addCollisionHandler (space gameData) collisionTypeEthereal collisionTypeNormal $ H.Handler
        (Just $ beginHandler' False etherealCollisions') Nothing Nothing Nothing
    where
    beginHandler' :: Bool -> IORef [(H.Shape, H.Shape)] -> H.Callback H.Begin Bool
    beginHandler' shouldCollide collisions' = do
        collision <- H.shapes
        liftIO $ modifyIORef collisions' ((:) collision)
        return shouldCollide

type Position = H.Position

positionToPoint :: H.Position -> Point
positionToPoint (H.Vector a b) = (double2Float a, double2Float b)

rectangleToCornerPoints :: Rect -> (Point, Point)
rectangleToCornerPoints ((x, y), (width, height)) = ((x - halfWidth, y - halfHeight), (x + halfWidth, y + halfHeight))
    where
    halfWidth = width/2
    halfHeight = height/2

setPosition :: H.Position -> Entity -> GameState ()
setPosition coordinate entity = do
    -- I am hiding that positions for non-collideables are different than
    -- positions for collideables
    gameData <- get
    let (PhysicsData body _ isStatic) = physicsState gameData Map.! entity
    if hasPhysics gameData entity
        then do
            liftIO $ H.position body S.$= coordinate
            when isStatic $ liftIO $ H.rehashStatic $ space gameData
        else do
            let transformState' = Map.insert entity coordinate $ transformState gameData
            put gameData{transformState = transformState'}
    return ()

setScale :: Float -> Float -> Entity -> GameState ()
setScale x y entity = do
    gameData <- get
    let scaleState' = Map.insert entity (x, y) $ scaleState gameData
    put gameData{scaleState = scaleState'}

setVelocity :: H.Velocity -> Entity -> GameState ()
setVelocity velocity entity = do
    gameData <- get
    let body = getBody gameData entity
    liftIO $ H.velocity body S.$= velocity

updatePhysics :: GameState ()
updatePhysics = do
    gameData <- get

    doaCollisions' <- liftIO $ readIORef $ doaCollisions gameData
    foldState doaCollide doaCollisions'
    liftIO $ writeIORef (doaCollisions gameData) []

    collisions' <- liftIO $ readIORef $ collisions gameData
    foldState collide collisions'
    liftIO $ writeIORef (collisions gameData) []

    etherealCollisions' <- liftIO $ readIORef $ etherealCollisions gameData
    foldState collide etherealCollisions'
    liftIO $ writeIORef (etherealCollisions gameData) []

collide :: (H.Shape, H.Shape) -> GameState ()
collide (shape1, shape2) = do
    gameData <- get
    let entity1 = shapes gameData Map.! shape1
    let entity2 = shapes gameData Map.! shape2
    liftIO $ print (entity1, entity2)

collisionTypeDOA :: H.CollisionType
collisionTypeDOA = 1

collisionTypeEthereal :: H.CollisionType
collisionTypeEthereal = 2

collisionTypeNormal :: H.CollisionType
collisionTypeNormal = 3

doaCollide :: (H.Shape, H.Shape) -> GameState ()
doaCollide (doaShape, normalShape) = do
    gameData <- get
    let entity1 = shapes gameData Map.! doaShape
    let entity2 = shapes gameData Map.! normalShape
    liftIO $ print ( "arrival", entity1, entity2)

withinBox :: Rect -> Entity -> GameState Bool
withinBox box entity = do
    gameData <- get
    entityPosition <- liftIO $ getPosition entity gameData
    let (leftCorner, rightCorner) = rectangleToCornerPoints box
    return $ pointInBox (positionToPoint entityPosition) leftCorner rightCorner
