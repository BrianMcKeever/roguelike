module Components.TransformBase (
    initialScaleState,
    initialTransformState,
    ScaleState,
    transformComponent,
    TransformState
)
where
import EntityComponentSystem
import qualified Data.Map.Lazy as Map
import qualified Physics.Hipmunk as H
import StringTable.Atom

initialScaleState :: ScaleState
initialScaleState = Map.empty

initialTransformState :: TransformState
initialTransformState = Map.empty

type ScaleState = Map.Map Serial (Float, Float)

transformComponent :: Component
transformComponent = Component 0 $ toAtom "transform"

type TransformState = Map.Map Serial H.Position
