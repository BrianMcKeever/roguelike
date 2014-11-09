module Components.Transform (
    addTransform,
    getPosition,
    getScale,
    hasTransform,
    positionToPoint,
    Position,
    setPosition,
    withinBox
)
where
import EntityComponentSystem
import Components.PhysicsBase
import Control.Monad.State.Lazy
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import qualified Data.StateVar as S
import GameState
import GHC.Float
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Game
import qualified Physics.Hipmunk as H

addTransform :: H.Position -> Float -> Float -> Entity -> GameState Entity
addTransform coordinate scaleX scaleY entity = do
    gameData <- get
    let components' = Set.insert entity $ transformComponents gameData
    put gameData{transformComponents = components'}
    setPosition coordinate entity
    setScale scaleX scaleY entity
    return entity

getPosition :: Entity -> GameState H.Position
getPosition entity = do
    gameData <- get
    let (PhysicsData body _) = physicsState gameData Map.! entity
    if hasComponent entity gameData physicsComponent
    then do 
        position <- liftIO $ S.get $ H.position body
        return position
    else return $ transformState gameData Map.! entity

getScale :: GameData -> Entity -> (Float, Float)
getScale gameData entity = scaleState gameData Map.! entity

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
    if hasComponent entity gameData physicsComponent
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

withinBox :: Rect -> Entity -> GameState Bool
withinBox box entity = do
    entityPosition <- getPosition entity
    let (leftCorner, rightCorner) = rectangleToCornerPoints box
    return $ pointInBox (positionToPoint entityPosition) leftCorner rightCorner
