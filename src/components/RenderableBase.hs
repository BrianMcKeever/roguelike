module Components.RenderableBase (
    RenderData(..),
    ZIndex(..)
)
where
import Graphics.Gloss.Game

data RenderData = RenderData ZIndex Picture

instance Eq RenderData where
    (RenderData index1 _) == (RenderData index2 _) = index1 == index2

instance Ord RenderData where
    compare (RenderData index1 _) (RenderData index2 _) = compare index1 index2

data ZIndex = Earth | Blood | Feet | Body | Head | Sky deriving (Eq, Ord)
