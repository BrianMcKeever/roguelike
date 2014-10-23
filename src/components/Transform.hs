module Components.Transform (
    addTransform,
    getPosition,
    Position,
    positionToPoint,
    removeTransform,
    setPosition,
    transformComponent,
    withinBox
)
where
import EntityComponentSystem
import Components.PhysicsBase
import Components.TransformBase
import qualified Data.Map.Lazy as Map
import Data.StateVar
import GameState
import GHC.Float
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Game
import qualified Physics.Hipmunk as H

addTransform :: H.Position -> GameState -> Entity -> IO (Entity, GameState)
addTransform coordinate gameState entity = do
    let (entity', gameState') = addComponent transformComponent gameState entity
    gameState'' <- setPosition coordinate entity' gameState'
    return (entity', gameState'')

getPosition :: GameState -> Entity -> IO H.Position
getPosition gameState entity@(Entity serial _ _) = if hasComponent entity physicsComponent
    then get $ H.position body
    else return $ (transformState gameState) Map.! serial
    where
    (PhysicsData body _) = physicsState gameState Map.! serial

rectangleToCornerPoints :: Rect -> (Point, Point)
rectangleToCornerPoints ((x, y), (width, height)) = (((x - halfWidth), (y - halfHeight)), ((x + halfWidth), (y + halfHeight)))
    where
    halfWidth = width/2
    halfHeight = height/2

removeTransform :: GameState -> Entity -> IO (Entity, GameState)
removeTransform gameState entity@(Entity serial _ _) = do
    return $ if hasComponent entity physicsComponent
    then error  "undefined removeTransform execution path"
    -- I'm not sure what this would involve. Sould I remove the physics
    -- component as well?
    else do
        let transformState' = Map.delete serial $ transformState gameState
        removeComponent transformComponent gameState{transformState = transformState'} entity
    
    
setPosition :: H.Position -> Entity -> GameState -> IO GameState
setPosition coordinate entity@(Entity serial _ _) gameState = do
    -- I am hiding that positions for non-collideables are different than
    -- positions for collideables
    if hasComponent entity physicsComponent
        then do 
            H.position body $= coordinate
            return gameState
        else do
            let transformState' = Map.insert serial coordinate $ transformState gameState
            return gameState{transformState = transformState'}
    where
    (PhysicsData body _) = physicsState gameState Map.! serial

type Position = H.Position

positionToPoint :: H.Position -> Point
positionToPoint (H.Vector a b) = (double2Float a, double2Float b)

withinBox :: GameState -> Rect -> Entity -> IO Bool
withinBox gameState box entity = do
    entityPosition <- getPosition gameState entity
    let (leftCorner, rightCorner) = rectangleToCornerPoints box
    return $ pointInBox (positionToPoint entityPosition) leftCorner rightCorner
