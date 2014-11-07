module Components.Transform (
    addTransform,
    getPosition,
    getScale,
    positionToPoint,
    Position,
    removeTransform,
    setPosition,
    transformComponent,
    withinBox
)
where
import EntityComponentSystem
import Components.PhysicsBase
import Components.TransformBase
import Control.Monad.State.Lazy
import qualified Data.Map.Lazy as Map
import qualified Data.StateVar as S
import GameState
import GHC.Float
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Game
import qualified Physics.Hipmunk as H

addTransform :: H.Position -> Float -> Float -> Entity -> GameState Entity
addTransform coordinate scaleX scaleY entity = do
    entity2 <- addComponent transformComponent entity
    setPosition coordinate entity2 
    setScale scaleX scaleY entity2
    return entity2

getPosition :: Entity -> GameState H.Position
getPosition entity@(Entity serial _ _) = do
    gameData <- get
    let (PhysicsData body _) = physicsState gameData Map.! serial
    if hasComponent entity physicsComponent
    then do 
        position <- liftIO $ S.get $ H.position body
        return position
    else return $ transformState gameData Map.! serial

getScale :: GameData -> Entity -> (Float, Float)
getScale gameData (Entity serial _ _) = scaleState gameData Map.! serial

type Position = H.Position

positionToPoint :: H.Position -> Point
positionToPoint (H.Vector a b) = (double2Float a, double2Float b)

rectangleToCornerPoints :: Rect -> (Point, Point)
rectangleToCornerPoints ((x, y), (width, height)) = ((x - halfWidth, y - halfHeight), (x + halfWidth, y + halfHeight))
    where
    halfWidth = width/2
    halfHeight = height/2

--This function should be removed
removeTransform :: Entity -> GameState Entity
removeTransform entity@(Entity serial _ _) =
    if hasComponent entity physicsComponent
    then error  "undefined removeTransform execution path"
    -- I'm not sure what this would involve. Sould I remove the physics
    -- component as well?
    else do
        gameData <- get
        put gameData{transformState = Map.delete serial $ transformState gameData}
        removeComponent transformComponent entity
    
setPosition :: H.Position -> Entity -> GameState ()
setPosition coordinate entity@(Entity serial _ _) = do
    -- I am hiding that positions for non-collideables are different than
    -- positions for collideables
    gameData <- get
    let (PhysicsData body _) = physicsState gameData Map.! serial
    if hasComponent entity physicsComponent
        then liftIO $ H.position body S.$= coordinate
        else do
            let transformState' = Map.insert serial coordinate $ transformState gameData
            put gameData{transformState = transformState'}
    return ()

setScale :: Float -> Float -> Entity -> GameState ()
setScale x y (Entity serial _ _) = do
    gameData <- get
    let scaleState' = Map.insert serial (x, y) $ scaleState gameData
    put gameData{scaleState = scaleState'}

withinBox :: Rect -> Entity -> GameState Bool
withinBox box entity = do
    entityPosition <- getPosition entity
    let (leftCorner, rightCorner) = rectangleToCornerPoints box
    return $ pointInBox (positionToPoint entityPosition) leftCorner rightCorner
