module Components.SimpleMovementBase (
    Destination(..),
    initialSimpleMovementState,
    simpleMovementComponent,
    SimpleMovementState
)
where
import EntityComponentSystem
import qualified Data.Map.Lazy as Map
import qualified Physics.Hipmunk as H
import StringTable.Atom

data Destination = Location H.Position | Nowhere deriving (Eq, Show)

initialSimpleMovementState :: SimpleMovementState
initialSimpleMovementState = Map.empty

simpleMovementComponent :: Component
simpleMovementComponent = Component 0 $ toAtom "simpleMovement"

type SimpleMovementState = Map.Map Serial Destination
