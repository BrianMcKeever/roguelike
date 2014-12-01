module EntityComponentSystem (
    Component,
    componentToMask,
    createMask,
    Entity,
    initialMasks,
    maskContains,
    Mask,
    Masks,
    maxEntities
)
where
import Data.Bits
import Data.List
import Data.Word
import qualified Data.Vector as Vector

data Component = EmptyMask | PhysicsMask | RenderableMask | SimpleMovementMask 
    deriving (Enum, Eq, Show)

componentToMask :: Component -> Mask
componentToMask component = if enum == 0 then 0 else shift 1 enum
    where
    enum = fromEnum component

createMask :: [Component] -> Mask
createMask = foldl' (\m c -> componentToMask c .|. m) 0

type Entity = Word16

initialMasks :: Masks
initialMasks = Vector.replicate maxEntities $ componentToMask EmptyMask

type Mask = Word32

--If the first argument contains the second argument, true. Otherwise, false
maskContains :: Mask -> Mask -> Bool
maskContains a b = a .&. b == b

type Masks = Vector.Vector Mask

maxEntities :: Int
maxEntities = 3000
