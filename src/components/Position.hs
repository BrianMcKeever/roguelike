module Components.Position (
    addPosition,
    Coordinate,
    initialPositionState,
    PositionState,
    setPosition
)
where
import EntityComponentSystem
import qualified Data.Map.Lazy as Map

addPosition :: Coordinate -> PositionState -> Entity -> (Entity, PositionState)
addPosition coordinate positionState (Entity serial components) = (entity', positionState')
    where
    entity' = Entity serial $ componentInsert positionComponent components
    positionState' = setPosition coordinate entity' positionState

type Coordinate = (Int, Int)

initialPositionState :: PositionState
initialPositionState = Map.empty

positionComponent :: Component
positionComponent = Component 0 "position"

type PositionState = Map.Map Serial Coordinate

setPosition :: Coordinate -> Entity -> PositionState -> PositionState
setPosition coordinate (Entity serial _) positionState = Map.insert serial coordinate positionState
