module Components.Position (
    addPosition,
    getPosition,
    initialPositionState,
    Point,
    positionComponent,
    PositionState,
    setPosition
)
where
import EntityComponentSystem
import Graphics.Gloss.Game
import Graphics.Gloss.Data.Point
import qualified Data.Map.Lazy as Map
import StringTable.Atom

addPosition :: Point -> PositionState -> Entity -> (Entity, PositionState)
addPosition coordinate positionState (Entity serial components) = (entity', positionState')
    where
    entity' = Entity serial $ componentInsert positionComponent components
    positionState' = setPosition coordinate entity' positionState

getPosition :: PositionState -> Entity -> Point
getPosition state (Entity serial components) = state Map.! serial

initialPositionState :: PositionState
initialPositionState = Map.empty

positionComponent :: Component
positionComponent = Component 0 $ toAtom "position"

type PositionState = Map.Map Serial Point

setPosition :: Point -> Entity -> PositionState -> PositionState
setPosition coordinate (Entity serial _) positionState = Map.insert serial coordinate positionState

