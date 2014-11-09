module Components.TransformBase (
    initialScaleState,
    initialTransformComponents,
    initialTransformState,
    ScaleState,
    TransformComponents,
    TransformState
)
where
import EntityComponentSystem
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import qualified Physics.Hipmunk as H

initialScaleState :: ScaleState
initialScaleState = Map.empty

initialTransformState :: TransformState
initialTransformState = Map.empty

initialTransformComponents :: TransformComponents
initialTransformComponents = Set.empty

type ScaleState = Map.Map Entity (Float, Float)

type TransformComponents = Set.Set Entity

type TransformState = Map.Map Entity H.Position
