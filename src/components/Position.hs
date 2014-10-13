module Components.Position (
    addPosition,
    Coordinate,
    createPositionState,
    PositionState,
    setPosition
)
where
import EntityComponentSystem
import qualified Data.Map.Lazy as Map

addPosition :: Coordinate -> World -> Entity -> (Entity, World)
addPosition coordinate world (Entity serial components) = (entity', world')
    where
    entity' = Entity serial $ componentInsert positionComponent components
    world' = setPosition coordinate entity' world

type Coordinate = (Int, Int)

initialPositionState :: PositionState
initialPositionState = Map.empty

positionComponent :: Component
positionComponent = Component 0 "position"

type PositionState = Map.Map Serial Coordinate

setPosition :: Coordinate -> Entity -> World -> World
setPosition coordinate (Entity serial _) world@{positionState = positionState'} = world {positionState = Map.insert serial coordinate positionState'}
