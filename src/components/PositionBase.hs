module Components.PositionBase (
    initialPositionState,
    Point,
    positionComponent,
    PositionState
)
where
import EntityComponentSystem
import Graphics.Gloss.Game
import qualified Data.Map.Lazy as Map
import StringTable.Atom

initialPositionState :: PositionState
initialPositionState = Map.empty

positionComponent :: Component
positionComponent = Component 0 $ toAtom "position"

type PositionState = Map.Map Serial Point
