module Components.TransformAndPhysics (
    addPhysics,
    addTransform,
    createSquare,
    getBody,
    getPosition,
    getScale,
    hasTransform,
    getVelocity,
    hasPhysics,
    positionToPoint,
    Position,
    setPosition,
    withinBox,
    setVelocity
)
where
import Components.PhysicsBase
import Control.Monad.State.Lazy
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import qualified Data.StateVar as S
import EntityComponentSystem
import GameState
import GHC.Float
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Game
import qualified Physics.Hipmunk as H

addPhysics :: H.Mass -> H.Moment -> H.ShapeType -> Entity -> Bool -> GameState Entity
addPhysics mass moment shapeType entity isStatic = do
    addComponent physicsComponent entity
    gameData <- get
    let space' = space gameData
    body <- if isStatic
        then liftIO $ H.newBody H.infinity H.infinity
            --TODO when it is possible to add static bodies, we need to add them
        else liftIO $ H.newBody mass moment
    liftIO $ H.spaceAdd space' body
    shape <- liftIO $ H.newShape body shapeType $ H.Vector 0 0
    liftIO $ H.spaceAdd space' shape

    -- I am assuming I will only be using simple shapes, so I'm defaulting the
    -- position offset to (0, 0)

    let physicsState' = Map.insert entity (PhysicsData body shape) $ physicsState gameData
    put gameData{physicsState = physicsState'}

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
    (PhysicsData body _) = physicsState gameData Map.! entity

getPosition :: Entity -> GameData -> IO H.Position
getPosition entity gameData = do
    let (PhysicsData body _) = physicsState gameData Map.! entity
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
    let (PhysicsData body _) = physicsState gameData Map.! entity
    if hasPhysics gameData entity
        then liftIO $ H.position body S.$= coordinate
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

withinBox :: Rect -> Entity -> GameState Bool
withinBox box entity = do
    gameData <- get
    entityPosition <- liftIO $ getPosition entity gameData
    let (leftCorner, rightCorner) = rectangleToCornerPoints box
    return $ pointInBox (positionToPoint entityPosition) leftCorner rightCorner
