module Components.Position (
    addPosition,
    getPosition,
    Point,
    positionComponent,
    setPosition,
    withinBox
)
where
import EntityComponentSystem
import GameState
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Game
import qualified Data.Map.Lazy as Map
import Components.PositionBase

addPosition :: Point -> GameState -> Entity -> (Entity, GameState)
addPosition coordinate gameState entity = (entity', gameState'')
    where
    (entity', gameState') = addComponent gameState entity positionComponent
    gameState'' = setPosition coordinate entity' gameState'

getPosition :: GameState -> Entity -> Point
getPosition gameState (Entity serial _ _) = (positionState gameState) Map.! serial

rectangleToCornerPoints :: Rect -> (Point, Point)
rectangleToCornerPoints ((x, y), (width, height)) = (((x - halfWidth), (y - halfHeight)), ((x + halfWidth), (y + halfHeight)))
    where
    halfWidth = width/2
    halfHeight = height/2

setPosition :: Point -> Entity -> GameState -> GameState
setPosition coordinate (Entity serial _ _) gameState = gameState{positionState = positionState'}
    where
    positionState' = Map.insert serial coordinate $ positionState gameState

withinBox :: GameState -> Rect -> Entity -> Bool
withinBox gameState box entity = pointInBox entityPosition leftCorner rightCorner
    where
    entityPosition = getPosition gameState entity
    (leftCorner, rightCorner) = rectangleToCornerPoints box
