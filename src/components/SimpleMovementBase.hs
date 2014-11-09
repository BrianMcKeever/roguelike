module Components.SimpleMovementBase (
    Destination(..),
    initialSimpleMovementState,
    SimpleMovementState
)
where
import EntityComponentSystem
import qualified Data.Map.Lazy as Map
import qualified Physics.Hipmunk as H

data Destination = Location H.Position | Nowhere deriving (Eq, Show)

initialSimpleMovementState :: SimpleMovementState
initialSimpleMovementState = Map.empty

type SimpleMovementState = Map.Map Entity Destination
